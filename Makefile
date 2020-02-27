modrian: mondrian.cmo sat_solver.cmo
	ocamlc -o mondrian graphics.cma sat_solver.cmo mondrian.cmo

sat_solver.cmo: sat_solver.ml sat_solver.cmi
	ocamlc -c sat_solver.ml

sat_solver.cmi: sat_solver.mli
	ocamlc sat_solver.mli

mondrian.cmo: mondrian.ml sat_solver.cmi
	ocamlc -c mondrian.ml

clean:
	rm -f *~ *.cm* mondrian
