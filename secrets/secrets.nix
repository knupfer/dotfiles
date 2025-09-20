let keys = [ "age1v9cz4expd4u2vp7dyh8zwlh0asy23lvj83lesm4gxz9sl5chzu5q5amwwf" ];
in
{
  knupferHashedPassword.age.publicKeys = keys;
  ramirezHashedPassword.age.publicKeys = keys;
  radicaleUsers.age.publicKeys = keys;
  radicaleKnupfer.age.publicKeys = keys;
}
