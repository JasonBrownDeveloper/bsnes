name := csnes
flags += -g

link += -fopenmp

# rules
objects += ui-csnes
objects := $(objects:%=obj/%.o)

obj/ui-csnes.o: $(ui)/csnes.cpp

# targets
all: $(objects)
	$(strip $(compiler) -o out/$(name) $(objects) $(link))
