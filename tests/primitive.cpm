assert 2 + 6 == 8;
assert -2 + 6 == 4;
assert 2 + -6 == -4;
assert -2 + -6 == -8;

assert 2 - 6 == -4;
assert -2 - 6 == -8;
assert 2 - -6 == 8;
assert -2 - -6 == 4;

assert 2 -6 == -4;
assert 2-6 == -4;

assert 2 * 6 == 12;
assert -2 * 6 == -12;
assert 2 * -6 == -12;
assert -2 * -6 == 12;

assert 6 / 2 == 3;
assert -6 / 2 == -3;
assert 6 / -2 == -3;
assert -6 / -2 == 3;

assert 2 / 6 == 0;
assert -2 / 6 == 0;
assert 2 / -6 == 0;
assert -2 / -6 == 0;

assert 6 % 5 == 1;
assert -6 % 5 == -1;
assert 6 % -5 == 1;
assert -6 % -5 == -1;

assert 1 + 2 * 3 == 7;
assert 1 * 2 + 3 == 5;
assert (1 + 2) * 3 == 9;



assert !false;
assert !!true;
assert !(!(true));

assert true || true;
assert true || false;
assert false || true;
assert !(false || false);

assert true && true;
assert !(true && false);
assert !(false && true);
assert !(false && false);

assert true == true;
assert false == false;
assert true != false;
assert false != true;

assert true || 10;
assert !(false && 10);

assert 10 > 5;
assert !(5 > 10);

assert 5 < 10;
assert !(10 < 5);

assert 10 >= 5;
assert 10 >= 10;

assert 5 <= 10;
assert 5 <= 5;

assert 5 == 5;
assert !(5 == 10);

assert 5 != 10;
assert !(5 != 5);

assert () == ();



a = "abc";
b = "123";
c = "\n";

str1 = "hello ";
str2 = "world";
str3 = "hello world";
assert str3 == str1 + str2;

str4 = 'hello ';
str5 = 'world';
str6 = 'hello world';
assert str4 + str5 == str6;

a = "abcdef";
assert a[3] == "d";
assert a[-1] == "f";
assert a[3:5] == "de";
assert a[-3:-1] == "de";
assert a[2:] == "cdef";
assert a[:-2] == "abcd";
assert a[:] == a;



assert (1, 2) == (1, 2);
assert (1, 2 + 3) == (1, 5);
assert (1, 2 + 3) != (1, 6);
assert (1, 1 + 1, 1 + 1 + 1) == (1, 2, 3);
assert 1,2 == (1,2);
assert (1,2) == 1,2;
assert 1 == (1);
assert (1) == 1;

assert (1, 2, 3)[0] == 1;
assert (1, 2, false)[2] == false;
x = (1,2); assert x[1] == 2;
