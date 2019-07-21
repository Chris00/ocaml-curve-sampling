open Printf

let () =
  let fh = open_out "latex_speed.tex" in
  fprintf fh "\\documentclass[12pt,a4paper]{article}\n\
              \\usepackage{tikz}\n\
              \\begin{document}\n\
              \\begin{tikzpicture}";
  let t = Curve_sampling.fn sin (-30.) 30. ~n:3000 in
  Curve_sampling.to_latex_channel t fh;
  fprintf fh "\\end{tikzpicture}\n\
              \\end{document}";
  close_out fh


(* Local Variables: *)
(* compile-command: "dune build latex_speed.exe" *)
(* End: *)
