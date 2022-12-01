implementation module _SystemStrictMaybes;

class Maybe .l e where {
	_cJust :: .e -> u:(l .e);
	_eJust :: !u:(l .e) -> .e;
};

class UMaybe e where {
	_cJust_u :: !.e -> u:(?# .e);
	_eJust_u :: !u:(?# .e) -> .e;
};

_none :: u:(l .e) | Maybe l e, [u<=e];
_none = code {
		pop_a 2
		fillh _None 0 0
	};

_none_u :: . ?# .e | UMaybe e;
_none_u = code {
		pop_a 2
		fillh _None 0 0
	};

instance Maybe ? a where {
	_cJust a = ?Just a;
	_eJust (?Just a) = a;
}

instance Maybe ?^ a where {
	_cJust a = ?^Just a;
	_eJust (?^Just a) = a;
}

instance Maybe ?# a | UMaybe a where {
	_cJust a = _cJust_u a;
	_eJust a = _eJust_u a;
}

instance UMaybe a where {
	_cJust_u a = code {
		print "_cJust_u of UMaybe a"
		halt
	};
	_eJust_u _ = code {
		print "_eJust_u of UMaybe a"
		halt
	}
}	

instance UMaybe Int where {
	_cJust_u a = code {
		fill_r _Justi 0 1 0 0 0
		pop_b 1
	};
	_eJust_u a = code {
		repl_r_args 0 1
	}
}

instance UMaybe Real where {
	_cJust_u a = code {
		fill_r _Justr 0 1 0 0 0
		pop_b 1
	};
	_eJust_u a = code {
		repl_r_args 0 1
	}
}

instance UMaybe Char where {
	_cJust_u a = code {
		fill_r _Justc 0 1 0 0 0
		pop_b 1
	};
	_eJust_u a = code {
		repl_r_args 0 1
	}
}

instance UMaybe Bool where {
	_cJust_u a = code {
		fill_r _Justb 0 1 0 0 0
		pop_b 1
	};
	_eJust_u a = code {
		repl_r_args 0 1
	}
}

instance UMaybe File where {
	_cJust_u a = code {
		fill_r _Justf 0 2 0 0 0
		pop_b 2
	};
	_eJust_u a = code {
		repl_r_args 0 2
	}
}

instance UMaybe {.a} where {
	_cJust_u a = code {
		fill_r _Justa 1 0 1 0 0
		pop_a 1
	};
	_eJust_u a = code {
		repl_r_args 1 0
	}
}

instance UMaybe {!.a} where {
	_cJust_u a = code {
		fill_r _Justa 1 0 1 0 0
		pop_a 1
	};
	_eJust_u a = code {
		repl_r_args 1 0
	}
}

instance UMaybe {#.a} where {
	_cJust_u a = code {
		fill_r _Justa 1 0 1 0 0
		pop_a 1
	};
	_eJust_u a = code {
		repl_r_args 1 0
	}
}

instance UMaybe {32#.a} where {
	_cJust_u a = code {
		fill_r _Justa 1 0 1 0 0
		pop_a 1
	};
	_eJust_u a = code {
		repl_r_args 1 0
	}
}
