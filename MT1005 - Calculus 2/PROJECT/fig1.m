syms r1 t1 r2 t2
x1 = r1*cos(t1);
y1 = r1*sin(t1);
z1 = -r1*cos(r1);
fs1 = fsurf(x1, y1, z1, [0 1.5*pi 0 2*pi]);
hold on
x2 = r2*cos(t2);
y2 = r2*sin(t2);
z2 = -sqrt(2.25*(pi).^2 - (r2).^2);
fs2 = fsurf(x2, y2, z2, [0 1.5*pi 0 2*pi]);
light
fs1.FaceAlpha = 0.5;
fs2.FaceAlpha = 0.5;
axis equal;
xlabel("$x$",Interpreter="latex");
ylabel("$y$",Interpreter="latex");
zlabel("$z$",Interpreter="latex");
picturewidth = 25; 
hw_ratio = 0.65; 