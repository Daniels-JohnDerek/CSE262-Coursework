program test(input, output);
const c1=1; c2=2; c3=3
var a, b, c, d : integer
begin
  a := 10;
  b := 11;
  c := 12;
  { this is a comment }
  while a < 15 do a := a + 1;
  if a >= 15 then
    d := 5
  else
    d := 6;
  repeat (* this one too *)
    a := a + 1
  until a >= 20;
  for i := 1 to 10 do begin
     b := b + 1;
     writeln(a:3,b:3,c:3,d:3)
  end
end.


