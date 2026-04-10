k = 5;
%figure 1
syms r1 t1 r2 t2 u v
x1 = r1*cos(t1);
y1 = k;
z1 = r1*sin(t1);
fs1 = fsurf(x1, y1, z1, [0 sqrt(1+(k.^2)./16) 0 2*pi]);
hold on
%figure 2
x2 = r2*cos(t2);
y2 = -k;
z2 = r2*sin(t2);
fs2 = fsurf(x2, y2, z2, [0 sqrt(1+(k.^2)./16) 0 2*pi]);
%figure 3
x3 = sqrt(1+(u.^2)./16).*cos(v);
y3 = u;
z3 = sqrt(1+(u.^2)./16).*sin(v);
fs3 = fsurf(x3,y3,z3, [-k k 0 2*pi]);
light
fs1.FaceAlpha = 0.5;
fs2.FaceAlpha = 0.5;
fs3.FaceAlpha = 0.75;
axis equal;
xlabel("$x$",Interpreter="latex");
ylabel("$y$",Interpreter="latex");
zlabel("$z$",Interpreter="latex");