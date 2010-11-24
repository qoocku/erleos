lincan_check = $(if $(LINCAN_ROOT), lincan, no_lincan)
test         = $(if $(etest), etest, no_etest)

all: $(lincan_check) defaults beams dlls $(test)
	
defaults: .def.erlroot .def.erts.vsn

.def.erlroot:
	${shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell > $@}
.def.erts.vsn:
	${shell erl -eval 'io:format("~s", [erlang:system_info(version)])' -s init stop -noshell > $@}

erts_vsn = ${shell cat .def.erts.vsn}
erl_root  = ${shell cat .def.erlroot}
dlls = ${foreach i,${notdir ${filter-out c_src/nif_utils.c,${wildcard c_src/*.c}}},priv/${i:.c=.so}}
beams = ${wildcard src/*.erl tests/*.erl}
CFLAGS = -Wall -I ${erl_root}/erts-${erts_vsn}/include -fpic -O3 -I ${LINCAN_ROOT}/include

etest:
	@erl -pa ebin -noshell -eval '$(etest)_tests:test()' -s init stop

no_etest:

lincan:
	
no_lincan:
	$(error LINCAN_ROOT parameter or environment variable should be set)

doc: ${wildcard src/*.erl}
	erl -pa ebin -noshell -eval 'edoc:application(erleos)' -s init stop

purge: clean
	rm -f .def.*
	
clean:
	rm -f ebin/*.beam
	rm -f c_src/*.o
	rm -f priv/*.so

beams: 
	@erl -make	
	
dlls: ${dlls}

priv/%.so: c_src/%.o
	${CC} -shared -o $@ $<
	
c_src/%.o: c_src/%.c 
