consult("res/swi/*.pl").
add(F, V, I, O, C).
add(f1:f2:f3, v, [f1:[f2:[]], a:b, f1:[f2:[]]], [f1:[f2:[f3:v]], a:b, f1:[f2:[f3:v]]], C).
add(f1:f2:f3, v, [f1:[f2:[]], a:b, f1:[f2:[]]], O, C).
add(f1:f2:f3, V, I, [f1:[f2:[f3:v]], a:b, f1:[f2:[f3:v]]], C).
add(x:y, z, I, [x:[y:z], a:b], C).
add(F, V:[1, 2, 3], I, O, C).
add(F, [1, 2, 3], [], O, C).
add(P, V, I, O, C).
add(P, V, I, [a:b], C).
add(P, V, I, [X:Y], C).
add(x:y, z, I, [x:[y:z]], C).
add(x:y, z, [x:[]], [x:[y:z]], 1).
add(X:Y, Z, [X:[]], [X:[Y:Z]], C).
add(F:Q, V, I, [X:[Y:Z]], C).
add(F, V, I, [X:[Y:Z]], C). 
add(P, V, I, [a:b], C).
add(P, V, I, [X:Y], C).
add(x:y, z, I, [x:[y:z]], C).
add(x:y, z, [x:[]], [x:[y:z]], 1).
add(X:Y, Z, [X:[]], [X:[Y:Z]], C).
add(F:Q, V, I, [X:[Y:Z]], C).
add(F, V, I, [X:[Y:Z]], C).
add(P, V, [], O, C).
add(P, V, [], [Q:V], C).
add(P, V, [], [a:b], C).
\+add(P, V, [a:b], [a:b, x:y], C).
add(P, V, [x:y], [a:b, x:y], C).
add(P, V, I, [a:b, x:y], C).
\+add(x, y, I, [a:b, x:y], C).
add(F:Q, [1, 2, 3], I, O, C).
add(F:Q:P, [1, 2, 3], I, O, C).
add(F:Q:P, [1, 2, 3], [], O, C).
add(F:Q:P, [1, 2, 3], I, O, C).
add(F1:F2:F3, V, [], O, C).
add(x:y, z, I, [x:[y:z], a:b], C).
add(x:y, z, I, O, C).
add(f1:f2:f3, V, I, [f1:[f2:[f3:v]], a:b, f1:[f2:[f3:v]]], C).
add(f1:f2:f3, V, I, [f1:[f2:[f3:v]], a:b, f1:[f2:[f3:w]]], C).
add(f1:f2:f3, v, [f1:[f2:[]], a:b, f1:[f2:[]]], [f1:[f2:[f3:v]], a:b, f1:[f2:[f3:v]]], C).
add(f1:f2:f3, V, [f1:[f2:[]], a:b, f1:[f2:[]]], [f1:[f2:[f3:v]], a:b, f1:[f2:[f3:v]]], C).
add(f1:f2:f3, v, [f1:[f2:[]], a:b, f1:[f2:[]]], O, C).
add(F, V:W, I, [X:[Y:Z]], C). 
add(f, [], [], O, C).
add(f, 12, [], O, C).
add(1, v, [], O, C).
add(f, a:b, [], O, C).
add(f1:f2, v, [], O, C).
add(f1:f2:f3, v, [], O, C).
add(f1:(f2:f3), v, [], O, C).
add((f1:f2):f3, v, [], O, C).
add(f, v, [f:v], O, C).
add(f, v, [f:v, x:y, a:b], O, C).
add(f1:f2, v, [f:v], O, C).
add(f1:f2, v, [f1:[]], O, C).
add(f1:f2, v, [f1:[x:y]], O, C).
add(f1:f2, v, [f1:[x:y], f1:[a:b]], O, C).
add(f1:f2:f3, v, [f1:[x:y], f1:[a:b]], O, C).
add(F, v, [], O, C).
add(F1:F2, v, [], O, C).
add(F1:F2:F3, v, [], O, C).
add(F1:(F2:F3), v, [], O, C).
add((F1:F2):F3, v, [], O, C).
add((F1:F2):F3, V, [], O, C).