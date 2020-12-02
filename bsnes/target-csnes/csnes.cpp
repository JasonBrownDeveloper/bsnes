#include <nall/string.hpp>
#include <nall/vfs.hpp>
#include <nall/any.hpp>
#include <nall/set.hpp>
#include <nall/chrono.hpp>

using namespace nall;

#include <emulator/emulator.hpp>
#include <sfc/sfc.hpp>

using namespace Emulator;
using namespace SuperFamicom;

#include <stdio.h>
#include "sglib.h"

#define BITMASK(b) (1 << ((b) % CHAR_BIT))
#define BITSLOT(b) ((b) / CHAR_BIT)
#define BITSET(a, b) ((a)[BITSLOT(b)] |= BITMASK(b))
#define BITCLEAR(a, b) ((a)[BITSLOT(b)] &= ~BITMASK(b))
#define BITTEST(a, b) ((a)[BITSLOT(b)] & BITMASK(b))
#define BITNSLOTS(nb) ((nb + CHAR_BIT - 1) / CHAR_BIT)

unsigned char ipl_rom[] = {
  0xcd, 0xef, 0xbd, 0xe8, 0x00, 0xc6, 0x1d, 0xd0, 0xfc, 0x8f, 0xaa, 0xf4,
  0x8f, 0xbb, 0xf5, 0x78, 0xcc, 0xf4, 0xd0, 0xfb, 0x2f, 0x19, 0xeb, 0xf4,
  0xd0, 0xfc, 0x7e, 0xf4, 0xd0, 0x0b, 0xe4, 0xf5, 0xcb, 0xf4, 0xd7, 0x00,
  0xfc, 0xd0, 0xf3, 0xab, 0x01, 0x10, 0xef, 0x7e, 0xf4, 0x10, 0xeb, 0xba,
  0xf6, 0xda, 0x00, 0xba, 0xf4, 0xc4, 0xf4, 0xdd, 0x5d, 0xd0, 0xdb, 0x1f,
  0x00, 0x00, 0xc0, 0xff
};
unsigned int ipl_rom_len = 64;

struct Program : Emulator::Platform {
  string program_rom;

  auto open(uint id, string name, vfs::file::mode mode, bool required = false) -> shared_pointer<vfs::file> {
    if(name == "manifest.bml") {
      const uint8_t manifest[] = {
"game\n"
"  sha256:   0938ff33f5bab359e383bb5499f4fcc2a488fe49747026db355c2d3d5c7c2fdb\n"
"  label:    クロノ・トリガー\n"
"  name:     Chrono Trigger\n"
"  region:   SHVC-ACTJ-JPN\n"
"  revision: SHVC-ACTJ-0\n"
"  board:    SHVC-BJ3M-20\n"
"    memory\n"
"      type: ROM\n"
"      size: 0x400000\n"
"      content: Program\n"
"    memory\n"
"      type: RAM\n"
"      size: 0x2000\n"
"      content: Save\n"
      };

      return vfs::memory::file::open(manifest, sizeof(manifest) - 1);
    } else if(name == "program.rom") {
      return vfs::fs::file::open(program_rom, vfs::file::mode::read);
    } else if(name == "ipl.rom") {
      return vfs::memory::file::open(ipl_rom, ipl_rom_len);
    } else if(name == "boards.bml") {
      const uint8_t boards[] = {
"board: SHVC-BJ3M-(10,20)\n"
"  memory type=ROM content=Program\n"
"    map address=00-3f,80-bf:8000-ffff\n"
"    map address=40-7d,c0-ff:0000-ffff\n"
"  memory type=RAM content=Save\n"
"    map address=20-3f,a0-bf:6000-7fff mask=0xe000\n"
      };

      return vfs::memory::file::open(boards, sizeof(boards) - 1);
    }

    return {};
  };

  auto load(uint id, string name, string type, vector<string> options = {}) -> Load {
    return {0, "NTSC"};
  }
};

void memory_map(const int addr, int *source, int *adjust) {
   int bank = addr >> 16;
   int page = addr & 0xFFFF;
   /* PPU */
   if ( ((bank >= 0x00 && bank <= 0x3f) || (bank >= 0x80 && bank <= 0xbf))
   &&           (page >= 0x2100 && page <= 0x213f) ) {
      *source = 'g';
      *adjust = page;
   /* APU */
   } else if ( ((bank >= 0x00 && bank <= 0x3f) || (bank >= 0x80 && bank <= 0xbf))
   &&           (page >= 0x2140 && page <= 0x217f) ) {
      *source = 'g';
      *adjust = page;
   /* CPU */
   } else if ( ((bank >= 0x00 && bank <= 0x3f) || (bank >= 0x80 && bank <= 0xbf))
   &&          ((page >= 0x2180 && page <= 0x2183) || (page >= 0x4016 && page <= 0x4017) || (page >= 0x4200 && page <= 0x421f)) ) {
      *source = 'g';
      *adjust = page;
   /* DMA */
   } else if ( ((bank >= 0x00 && bank <= 0x3f) || (bank >= 0x80 && bank <= 0xbf))
   &&           (page >= 0x4300 && page <= 0x437f) ) {
      *source = 'g';
      *adjust = page;
   /* WRAM */
   } else if ( ((bank >= 0x00 && bank <= 0x3f) || (bank >= 0x80 && bank <= 0xbf))
   &&           (page >= 0x0000 && page <= 0x1fff) ) {
      *source = 'w';
      *adjust = page;
   } else if ( (bank >= 0x7e && bank <= 0x7f)
   &&          (page >= 0x0000 && page <= 0xffff) ) {
      *source = 'w';
      *adjust = addr - 0x7e0000;
   /* CART */
   } else if ( (bank >= 0x00 && bank <= 0x3f)
   &&   (page >= 0x8000 && page <= 0xffff) ) {
      *source = 'r';
      *adjust = addr;
   } else if ( (bank >= 0x80 && bank <= 0xbf)
   &&          (page >= 0x8000 && page <= 0xffff) ) {
      *source = 'r';
      *adjust = addr - 0x800000;
   } else if ( (bank >= 0x40 && bank <= 0x7d)
   &&          (page >= 0x0000 && page <= 0xffff) ) {
      *source = 'r';
      *adjust = addr - 0x400000;
   } else if ( (bank >= 0xc0 && bank <= 0xff)
   &&          (page >= 0x0000 && page <= 0xffff) ) {
      *source = 'r';
      *adjust = addr - 0xc00000;
   } else if ( ((bank >= 0x10 && bank <= 0x3f) || (bank >= 0x90 && bank <= 0xbf))
   &&           (page >= 0x6000 && page <= 0x7fff) ) {
      *source = 's';
      *adjust = (addr - 0x6000) & 0xffff;
   } else {
      *source = 'u';
      *adjust = addr;
   }
}

#define CODE_SIZE (4 * 1024 * 1024)
#define CODE_COMPARATOR(e1, e2) ( (e1)->caddr != (e2)->caddr ? (e1)->caddr - (e2)->caddr \
                                : (e1)->cmap != (e2)->cmap ? (e1)->cmap - (e2)->cmap \
                                : (e1)->m != (e2)->m ? (e1)->m - (e2)->m \
                                : (e1)->x != (e2)->x ? (e1)->x - (e2)->x \
                                : 0 )

typedef struct code {
   struct code *next;
   int cmap;
   int caddr;
   int m;
   int x;
} code;

unsigned int code_hash_function(code *e) {
   return e->caddr;
}

SGLIB_DEFINE_SORTED_LIST_PROTOTYPES(code, CODE_COMPARATOR, next)
SGLIB_DEFINE_SORTED_LIST_FUNCTIONS(code, CODE_COMPARATOR, next)
SGLIB_DEFINE_HASHED_CONTAINER_PROTOTYPES(code, CODE_SIZE, code_hash_function)
SGLIB_DEFINE_HASHED_CONTAINER_FUNCTIONS(code, CODE_SIZE, code_hash_function)

string cpu_buffer;
string event_buffer;

code cfind, *caccess;
code *ctab[CODE_SIZE];

int cached_pc;

auto cpu_trace(uint24 addr) -> void {
   printf("%s\n%s", (const char*)cpu_buffer, (const char*)event_buffer);
   //fflush(stdout);
   event_buffer.reset();

   cached_pc = addr;
   cpu_buffer = cpu.disassemble();
   cpu_buffer.append(" V:{0} H:{1}", string_format{cpu.vcounter(), cpu.hcounter()});

   string effective = cpu_buffer.slice(24, 6);
   if(effective == "002180") {
       string pre = cpu_buffer.slice(0,24);
       string post = cpu_buffer.slice(30);
       char effective[7];
       sprintf(effective, "%06x", 0x7e0000 + cpu.wramAddress);
       cpu_buffer = pre.append(effective).append(post);
   }
   memory_map(cached_pc, &cfind.cmap, &cfind.caddr);

   cfind.m = cpu.r.p.m;
   cfind.x = cpu.r.p.x;
   if (sglib_hashed_code_find_member(ctab, &cfind) == NULL) {
      caccess = new struct code;
      *caccess = cfind;
      sglib_hashed_code_add(ctab, caccess);
   }
};

#define DATA_SIZE (128 * 1024)
#define DATA_COMPARATOR(e1, e2) ( (e1)->daddr != (e2)->daddr ? (e1)->daddr - (e2)->daddr \
                                : (e1)->dmap != (e2)->dmap ? (e1)->dmap - (e2)->dmap \
                                : (e1)->caddr != (e2)->caddr ? (e1)->caddr - (e2)->caddr \
                                : (e1)->cmap != (e2)->cmap ? (e1)->cmap - (e2)->cmap \
                                : (e1)->read != (e2)->read ? (e1)->read - (e2)->read \
                                : 0 )

typedef struct data {
   struct data *next;
   int dmap;
   int daddr;
   int cmap;
   int caddr;
   int read;
} data;

unsigned int data_hash_function(data *e) {
   return e->daddr;
}

SGLIB_DEFINE_SORTED_LIST_PROTOTYPES(data, DATA_COMPARATOR, next)
SGLIB_DEFINE_SORTED_LIST_FUNCTIONS(data, DATA_COMPARATOR, next)
SGLIB_DEFINE_HASHED_CONTAINER_PROTOTYPES(data, DATA_SIZE, data_hash_function)
SGLIB_DEFINE_HASHED_CONTAINER_FUNCTIONS(data, DATA_SIZE, data_hash_function)

char WRAMInit[BITNSLOTS(DATA_SIZE)];

data dfind, *daccess;
data *dtab[DATA_SIZE];

auto read_tracer(uint24 addr, uint8 data) -> void {
   // Should probably use exact op size here instead
   if (addr >= cached_pc && addr <= cached_pc + 4)
      return;

   if (addr >= cpu.r.pc.d && addr <= cpu.r.pc.d + 4)
      return;

   if (addr >= cpu.r.vector && addr <= cpu.r.vector + 1)
      return;

   if (addr == cpu.r.s.w)
      return;

   memory_map(addr, &dfind.dmap, &dfind.daddr);

   if (dfind.dmap == 'g' && dfind.daddr == 0x2180) {
      dfind.dmap = 'w';
      dfind.daddr = cpu.wramAddress - 1;
   }

   if (dfind.dmap == 'w' && !BITTEST(WRAMInit, dfind.daddr)) {
      event_buffer.append(" Uninitialized WRAM !\n");
   }

   if (dfind.dmap == 'g'
   && (((dfind.daddr & 0x00ffff) >= 0x004218 && (dfind.daddr & 0x00ffff) <= 0x00421f)
    || ((dfind.daddr & 0x00ffff) >= 0x004016 && (dfind.daddr & 0x00ffff) <= 0x004017))) {
      event_buffer.append(" Input Read !\n");
   }

   dfind.cmap = cfind.cmap;
   dfind.caddr = cfind.caddr;
   dfind.read = true;
   if (sglib_hashed_data_find_member(dtab, &dfind) == NULL) {
      daccess = new struct data;
      *daccess = dfind;
      sglib_hashed_data_add(dtab, daccess);
   }
}

int cached_dma, cached_hdma;
auto write_tracer(uint24 addr, uint8 data) -> void {
   if (addr == cpu.r.s.w)
      return;

   memory_map(addr, &dfind.dmap, &dfind.daddr);

   if (dfind.dmap == 'g' && dfind.daddr == 0x420b)
      cached_dma = cached_pc;

   if (dfind.dmap == 'g' && dfind.daddr == 0x420c)
      cached_hdma = cached_pc;

   if (dfind.dmap == 'g' && dfind.daddr == 0x2180) {
      dfind.dmap = 'w';
      dfind.daddr = cpu.wramAddress - 1;
   }

   if (dfind.dmap == 'w')
      BITSET(WRAMInit, dfind.daddr);

   dfind.cmap = cfind.cmap;
   dfind.caddr = cfind.caddr;
   dfind.read = false;
   if (sglib_hashed_data_find_member(dtab, &dfind) == NULL) {
      daccess = new struct data;
      *daccess = dfind;
      sglib_hashed_data_add(dtab, daccess);
   }
}

string dma_buffer;
auto dma_tracer(bool direction, uint8 bbus, uint24 abus, uint8 data) -> void {
   dma_buffer = cpu.disassembleDma();
   dma_buffer.append(" V:{0} H:{1}", string_format{cpu.vcounter(), cpu.hcounter()});
   printf("%s\n", (const char *)dma_buffer);
   //fflush(stdout);

   memory_map(abus, &dfind.dmap, &dfind.daddr);

   if (dfind.dmap == 'g' && dfind.daddr == 0x2180) {
      dfind.dmap = 'w';
      dfind.daddr = cpu.wramAddress - 1;
   }

   if (direction == 0) {
      if (dfind.dmap == 'w' && !BITTEST(WRAMInit, dfind.daddr)) {
         puts(" Uninitialized WRAM !");
         //fflush(stdout);
      }

      if (dfind.dmap == 'g'
      && (((dfind.daddr & 0x00ffff) >= 0x004218 && (dfind.daddr & 0x00ffff) <= 0x00421f)
       || ((dfind.daddr & 0x00ffff) >= 0x004016 && (dfind.daddr & 0x00ffff) <= 0x004017))) {
         puts(" Input Read !");
      }

      if (dma_buffer[0] == 'd')
         memory_map(cached_dma, &dfind.cmap, &dfind.caddr);
      else
         memory_map(cached_hdma, &dfind.cmap, &dfind.caddr);

      dfind.read = true;
      if (sglib_hashed_data_find_member(dtab, &dfind) == NULL) {
         daccess = new struct data;
         *daccess = dfind;
         sglib_hashed_data_add(dtab, daccess);
      }

      dfind.dmap = 'g';
      dfind.daddr = 0x2100 | bbus;
      dfind.read = false;
      if (sglib_hashed_data_find_member(dtab, &dfind) == NULL) {
         daccess = new struct data;
         *daccess = dfind;
         sglib_hashed_data_add(dtab, daccess);
      }
   } else {
      if (dfind.dmap == 'w')
         BITSET(WRAMInit, dfind.daddr);

      if (dma_buffer[0] == 'd')
         memory_map(cached_dma, &dfind.cmap, &dfind.caddr);
      else
         memory_map(cached_hdma, &dfind.cmap, &dfind.caddr);

      dfind.read = false;
      if (sglib_hashed_data_find_member(dtab, &dfind) == NULL) {
         daccess = new struct data;
         *daccess = dfind;
         sglib_hashed_data_add(dtab, daccess);
      }

      dfind.dmap = 'g';
      dfind.daddr = 0x2100 | bbus;
      dfind.read = true;
      if (sglib_hashed_data_find_member(dtab, &dfind) == NULL) {
         daccess = new struct data;
         *daccess = dfind;
         sglib_hashed_data_add(dtab, daccess);
      }
   }
}

auto nmi_tracer() -> void {
   cached_pc = cpu.r.pc.d;
   memory_map(cached_pc, &cfind.cmap, &cfind.caddr);

   cfind.m = cpu.r.p.m;
   cfind.x = cpu.r.p.x;
}

auto irq_tracer() -> void {
   cached_pc = cpu.r.pc.d;
   memory_map(cached_pc, &cfind.cmap, &cfind.caddr);

   cfind.m = cpu.r.p.m;
   cfind.x = cpu.r.p.x;
}

int main(int argc, char *argv[]) {
  if(argc < 2) return 1;

  Program *program = new Program;
  program->program_rom = argv[1];

  Emulator::Interface* emulator = new SuperFamicom::Interface;
  platform = program;

  cpu.debugger.execute = cpu_trace;
  cpu.debugger.read = read_tracer;
  cpu.debugger.write = write_tracer;
  cpu.debugger.dma = dma_tracer;

  emulator->load();
  emulator->power();
  for(int i = 0; i < 1000; ++i) {
    emulator->run();
  }

  struct sglib_hashed_code_iterator cit;
  auto cfile = file::open({"codemap.txt"}, file::mode::write);
  for (caccess = sglib_hashed_code_it_init(&cit, ctab); caccess != NULL; caccess = sglib_hashed_code_it_next(&cit)) {
    int cmap;
    switch(caccess->cmap) {
      case 'r': cmap = 1; break;
      case 'w': cmap = 2; break;
      case 's': cmap = 3; break;
      case 'g': cmap = 5; break;
      default:  cmap = 0; break;
    }
    cfile.writes(string{"{0}\t{1}\t{2}\t{3}\n", string_format{cmap
                                             , caccess->caddr
                                             , caccess->m
                                             , caccess->x}});
  }
  cfile.close();

  struct sglib_hashed_data_iterator dit;
  auto dfile = file::open({"datamap.txt"}, file::mode::write);
  for (daccess = sglib_hashed_data_it_init(&dit, dtab); daccess != NULL; daccess = sglib_hashed_data_it_next(&dit)) {
    int dmap;
    switch(daccess->dmap) {
      case 'r': dmap = 1; break;
      case 'w': dmap = 2; break;
      case 's': dmap = 3; break;
      case 'g': dmap = 5; break;
      default:  dmap = 0; break;
    }
    int cmap;
    switch(daccess->cmap) {
      case 'r': cmap = 1; break;
      case 'w': cmap = 2; break;
      case 's': cmap = 3; break;
      case 'g': cmap = 5; break;
      default:  cmap = 0; break;
    }
    dfile.writes(string{"{0}\t{1}\t{2}\t{3}\t{4}\n", string_format{dmap
                                                  , daccess->daddr
                                                  , cmap
                                                  , daccess->caddr
                                                  , daccess->read}});
  }
  dfile.close();

  return 0;
}
