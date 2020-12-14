assert (1, 2 + 3) == (1, 5);
assert (1, 2 + 3) != (1, 6);
assert (1, 1 + 1, 1 + 1 + 1) == (1, 2, 3);

assert (1, 2, 3)[0] == 1;
assert (1, 2, false)[2] == false;

assert 1,2 == (1,2);
assert 1 == (1);

x = (1,2); assert x[1] == 2;

a, b = 10, 20; assert (a == 10 && b == 20);
a, b, c = 1, 2, a; assert (a == 1 && b == 2 && c == 10);

