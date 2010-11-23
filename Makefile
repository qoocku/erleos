lincan_check = $(if $(LINCAN_ROOT), lincan, no_lincan)

all: $(lincan_check) defaults beams dlls
	
defaults: .def.erlroot .def.erts.vsn

.def.erlroot:
	${shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell > $@}
.def.erts.vsn:
	${shell erl -eval 'io:format("~s", [erlang:system_info(version)])' -s init stop -noshell > $@}

erts_vsn = ${shell cat .def.erts.vsn}
erl_root  = ${shell cat .def.erlroot}
dlls = ${foreach i,${notdir ${filter-out c_src/nif_utils.c,${wildcard c_src/*.c}}},priv/${i:.c=.so}}
CFLAGS = -Wall -I ${erl_root}/erts-${erts_vsn}/include -fpic -O3 -I ${LINCAN_ROOT}/include

lincan:
	
no_lincan:
	$(error LINCAN_ROOT parameter or environment variable should be set)
	
clean:
	rm -f .def.*
	rm -f ebin/*.beam
	rm -f c_src/*.o
	rm -f priv/*.so

beams:
	@erl -make

dlls: ${dlls}

priv/%.so: c_src/%.o
	${CC} -shared -o $@ $<
	
c_src/%.o: c_src/%.c 
