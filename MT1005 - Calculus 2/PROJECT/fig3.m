hfig = figure;
N = 80;
phi = linspace(0, 2*pi, N);
theta = linspace(0, pi, N);
%Object
[Phi, Theta] = meshgrid(phi, theta);
R = 1 - cos(Theta);
X = R.*sin(Theta).*cos(Phi);
Y = R.*sin(Theta).*sin(Phi);
Z = R.*cos(Theta);
mesh(X, Y, Z, 'EdgeColor','r');
hold on
% Curve
R = 2*sin(theta);
Phi = 0;
X = R.*sin(theta).*cos(Phi);
Y = R.*sin(theta).*sin(Phi);
Z = R.*cos(theta);
plot3(X, Y, Z, 'g');
axis equal;
xlabel("$x$",Interpreter="latex");
ylabel("$y$",Interpreter="latex");
zlabel("$z$",Interpreter="latex");