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
assert (10, 5) == (10, 5);
