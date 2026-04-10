S = 0;
i = 0;
a = input('Nhap mang a: ');
n = input('Nhap so mat cat n: ');
if n == 1
    disp('Sai! n phai lon hon 1');
end
while i < n
    y = -a + ((2.*a)./(n-1)).*i;
    S = S + (pi).*(1 + (y.^2)./(16));
    i = i + 1;
end
fprintf('Dien tich %d mat cat', n);
format long;
disp(S);