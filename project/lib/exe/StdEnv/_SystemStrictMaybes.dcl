definition module _SystemStrictMaybes;

class Maybe .l e where {
	_cJust :: .e -> u:(l .e);
	_eJust :: !u:(l .e) -> .e;
};

class UMaybe e where {
	_cJust_u :: !.e -> u:(?# .e);
	_eJust_u :: !u:(?# .e) -> .e;
};

_none :: u:(l .e) | Maybe l e, [u<=e];
_none_u :: . ?# .e | UMaybe e;

instance Maybe ? a;
instance Maybe ?^ a;

instance Maybe ?# a | UMaybe a;

instance UMaybe a;

instance UMaybe Int;
instance UMaybe Real;
instance UMaybe Char;
instance UMaybe Bool;
instance UMaybe File;
instance UMaybe {.a};
instance UMaybe {!.a};
instance UMaybe {#.a};
instance UMaybe {32#.a};
