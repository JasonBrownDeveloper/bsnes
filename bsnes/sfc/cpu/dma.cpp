auto CPU::dmaEnable() -> bool {
  for(auto& channel : channels) if(channel.dmaEnable) return true;
  return false;
}

auto CPU::hdmaEnable() -> bool {
  for(auto& channel : channels) if(channel.hdmaEnable) return true;
  return false;
}

auto CPU::hdmaActive() -> bool {
  for(auto& channel : channels) if(channel.hdmaActive()) return true;
  return false;
}

auto CPU::dmaRun() -> void {
  counter.dma += 8;
  step<8,0>();
  dmaEdge();
  for(auto& channel : channels)
  for(status.channelN = 0; status.channelN < 8; status.channelN++) {
    channels[status.channelN].dmaRun();
  }
  status.irqLock = true;
}

auto CPU::hdmaReset() -> void {
  for(auto& channel : channels) channel.hdmaReset();
}

auto CPU::hdmaSetup() -> void {
  counter.dma += 8;
  step<8,0>();
  for(auto& channel : channels) channel.hdmaSetup();
  status.irqLock = true;
}

auto CPU::hdmaRun() -> void {
  counter.dma += 8;
  step<8,0>();
  for(status.channelN = 0; status.channelN < 8; status.channelN++) {
    channels[status.channelN].hdmaTransfer();
  }
  for(auto& channel : channels) channel.hdmaAdvance();
  status.irqLock = true;
}

//

template<uint Clocks, bool Synchronize>
auto CPU::Channel::step() -> void {
  cpu.counter.dma += Clocks;
  cpu.step<Clocks, Synchronize>();
}

auto CPU::Channel::edge() -> void {
  cpu.dmaEdge();
}

auto CPU::Channel::validA(uint24 address) -> bool {
  //A-bus cannot access the B-bus or CPU I/O registers
  if((address & 0x40ff00) == 0x2100) return false;  //00-3f,80-bf:2100-21ff
  if((address & 0x40fe00) == 0x4000) return false;  //00-3f,80-bf:4000-41ff
  if((address & 0x40ffe0) == 0x4200) return false;  //00-3f,80-bf:4200-421f
  if((address & 0x40ff80) == 0x4300) return false;  //00-3f,80-bf:4300-437f
  return true;
}

auto CPU::Channel::readA(uint24 address) -> uint8 {
  step<4,1>();
  cpu.r.mdr = validA(address) ? bus.read(address, cpu.r.mdr) : (uint8)0x00;
  step<4,1>();
  return cpu.r.mdr;
}

auto CPU::Channel::readB(uint8 address, bool valid) -> uint8 {
  step<4,1>();
  cpu.r.mdr = valid ? bus.read(0x2100 | address, cpu.r.mdr) : (uint8)0x00;
  step<4,1>();
  return cpu.r.mdr;
}

auto CPU::Channel::writeA(uint24 address, uint8 data) -> void {
  if(validA(address)) bus.write(address, data);
}

auto CPU::Channel::writeB(uint8 address, uint8 data, bool valid) -> void {
  if(valid) bus.write(0x2100 | address, data);
}

auto CPU::Channel::transfer(uint24 addressA, uint2 index) -> void {
  uint8 addressB = targetAddress;
  switch(transferMode) {
  case 1: case 5: addressB += index.bit(0); break;
  case 3: case 7: addressB += index.bit(1); break;
  case 4: addressB += index; break;
  }

  //transfers from WRAM to WRAM are invalid
  bool valid = addressB != 0x80 || ((addressA & 0xfe0000) != 0x7e0000 && (addressA & 0x40e000) != 0x0000);

  cpu.r.mar = addressA;
  if(direction == 0) {
    auto data = readA(addressA);
    writeB(addressB, data, valid);
  } else {
    auto data = readB(addressB, valid);
    writeA(addressA, data);
  }
  cpu.debugger.dma(direction, addressB, addressA, cpu.r.mdr);
}

auto CPU::Channel::dmaRun() -> void {
  if(!dmaEnable) return;

  step<8,0>();
  edge();

  do {
    transfer(sourceBank << 16 | sourceAddress, index++);
    if(!fixedTransfer) !reverseTransfer ? sourceAddress++ : sourceAddress--;
    edge();
  } while(dmaEnable && --transferSize);

  dmaEnable = false;
}

auto CPU::Channel::hdmaActive() -> bool {
  return hdmaEnable && !hdmaCompleted;
}

auto CPU::Channel::hdmaFinished() -> bool {
  auto channel = next;
  while(channel) {
    if(channel->hdmaActive()) return false;
    channel = channel->next;
  }
  return true;
}

auto CPU::Channel::hdmaReset() -> void {
  hdmaCompleted = false;
  hdmaDoTransfer = false;
}

auto CPU::Channel::hdmaSetup() -> void {
  hdmaDoTransfer = true;  //note: needs hardware verification
  if(!hdmaEnable) return;

  dmaEnable = false;  //HDMA will stop active DMA mid-transfer
  hdmaAddress = sourceAddress;
  lineCounter = 0;
  hdmaReload();
}

auto CPU::Channel::hdmaReload() -> void {
  auto data = readA(cpu.r.mar = sourceBank << 16 | hdmaAddress);

  if((uint7)lineCounter == 0) {
    lineCounter = data;
    hdmaAddress++;

    hdmaCompleted = lineCounter == 0;
    hdmaDoTransfer = !hdmaCompleted;

    if(indirect) {
      data = readA(cpu.r.mar = sourceBank << 16 | hdmaAddress++);
      indirectAddress = data << 8 | 0x00;  //todo: should 0x00 be indirectAddress >> 8 ?
      if(hdmaCompleted && hdmaFinished()) return;

      data = readA(cpu.r.mar = sourceBank << 16 | hdmaAddress++);
      indirectAddress = data << 8 | indirectAddress >> 8;
    }
  }
}

auto CPU::Channel::hdmaTransfer() -> void {
  if(!hdmaActive()) return;
  dmaEnable = false;  //HDMA will stop active DMA mid-transfer
  if(!hdmaDoTransfer) return;

  static const uint lengths[8] = {1, 2, 2, 4, 4, 4, 2, 4};
  for(index = 0; index < lengths[transferMode]; ++index) {
    uint24 address = !indirect ? sourceBank << 16 | hdmaAddress++ : indirectBank << 16 | indirectAddress++;
    transfer(address, index);
  }
}

auto CPU::Channel::hdmaAdvance() -> void {
  if(!hdmaActive()) return;
  lineCounter--;
  hdmaDoTransfer = bool(lineCounter & 0x80);
  hdmaReload();
}

auto CPU::disassembleDma() -> string {
  auto n = status.channelN;
  uint addressA, addressB;

  if(channels[n].dmaEnable) {
    addressA = channels[n].sourceBank << 16 | channels[n].sourceAddress;
    //if(!channels[n].fixedTransfer) !channels[n].reverseTransfer ? addressA-- : addressA++;
    uint2 indexB = channels[n].index - 1;
    uint8 addressB = channels[n].targetAddress;
    switch(channels[n].transferMode) {
    case 1: case 5: addressB += indexB.bit(0); break;
    case 3: case 7: addressB += indexB.bit(1); break;
    case 4: addressB += indexB; break;
    }
  } else {
    addressA = !channels[n].indirect ? channels[n].sourceBank << 16 | channels[n].hdmaAddress - 1 : channels[n].indirectBank << 16 | channels[n].indirectAddress - 1;
    uint2 indexB = channels[n].index;
    uint8 addressB = channels[n].targetAddress;
    switch(channels[n].transferMode) {
    case 1: case 5: addressB += indexB.bit(0); break;
    case 3: case 7: addressB += indexB.bit(1); break;
    case 4: addressB += indexB; break;
    }
  }

  return disassembleDma(n, channels[n].direction, addressB, addressA, r.mdr);
}

auto CPU::disassembleDma(uint n, bool direction, uint8 bbus, uint24 abus, uint8 data) -> string {
  string s;

  s = {"{0} {1}   {2} [{3}] {4} [{5}]", string_format{
    channels[n].dmaEnable ? "d" : "h",
    n,
    hex(data,2),
    bbus == 0x80 ? hex(0x7e0000 | wramAddress - 1, 6) : hex(0x2100 | bbus, 6),
    direction ? "->" : "<-",
    hex(abus,6)
  }};

  s.append(" A:{0} X:{1} Y:{2} S:{3} D:{4} B:{5} ", string_format{
    hex(r.a.w, 4), hex(r.x.w, 4), hex(r.y.w, 4),
    hex(r.s.w, 4), hex(r.d.w, 4), hex(r.b, 2)
  });

  if(r.e) {
    s.append(
      r.p.n ? 'N' : 'n', r.p.v ? 'V' : 'v',
      r.p.m ? '1' : '0', r.p.x ? 'B' : 'b',
      r.p.d ? 'D' : 'd', r.p.i ? 'I' : 'i',
      r.p.z ? 'Z' : 'z', r.p.c ? 'C' : 'c'
    );
  } else {
    s.append(
      r.p.n ? 'N' : 'n', r.p.v ? 'V' : 'v',
      r.p.m ? 'M' : 'm', r.p.x ? 'X' : 'x',
      r.p.d ? 'D' : 'd', r.p.i ? 'I' : 'i',
      r.p.z ? 'Z' : 'z', r.p.c ? 'C' : 'c'
    );
  }

  return s;
}
