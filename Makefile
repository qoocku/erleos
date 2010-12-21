eunit_tests  := $(if $(eunit), eunit, no_eunit)

all: $(lincan_check) defaults depsdir beams dlls $(eunit_tests)
	
defaults: .def.erlroot .def.erts.vsn

depsdir:
	@if [ ! -d deps ] ; then mkdir deps ; fi

-include makerl.config
-include makerl.deps

.def.erlroot:
	${shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell > $@}
.def.erts.vsn:
	${shell erl -eval 'io:format("~s", [erlang:system_info(version)])' -s init stop -noshell > $@}

erts_vsn = ${shell cat .def.erts.vsn}
erl_root  = ${shell cat .def.erlroot}
dlls = ${foreach i,${notdir ${filter-out c_src/nif_utils.c,${wildcard c_src/*.c}}},priv/${i:.c=.so}}
beams = ${wildcard src/*.erl tests/*.erl}
CFLAGS = -Wall -I ${erl_root}/erts-${erts_vsn}/include -fpic -O3 -I ${LINCAN_ROOT}/include

eunit:
	@erl -env ERL_LIBS deps -pa ebin -noshell -eval 'eunit:test(${subst _tests,,$(eunit:.erl=)}_tests, [verbose])' -s init stop

eunit-all: ${foreach b,${wildcard ebin/*_tests.beam}, $(b)_doit}
		
eunit-deps: ${foreach b,${wildcard deps/*/ebin/*_tests.beam}, $(b)_doit}

no_eunit:

no_deps_eunit:

doc: ${wildcard src/*.erl}
	erl -pa ebin -noshell -eval 'edoc:application(erleos, ".", [{preprocess, true}, {includes, "include"}])' -s init stop

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

${foreach b,${wildcard ebin/*_tests.beam}, $(b)_doit}:
	@erl -env ERL_LIBS deps -pa ebin -noshell -eval "eunit:test('${notdir $(@:.beam_doit=)}', [verbose])" -s init stop
	
${foreach b,${wildcard deps/*/ebin/*_tests.beam}, $(b)_doit}:
	@erl -env ERL_LIBS deps -noshell -eval "eunit:test('${notdir $(@:.beam_doit=)}', [verbose])" -s init stop ; fi	