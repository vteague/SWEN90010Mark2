module Storage

sig Item { }

sig State {
	contents : set Item,
    capacity : set Item
}

fact {
   all s : State |  #s.contents >= 0 and s.capacity >= 0
}

pred invariant [s : State] {
    #s.contents <= #s.capacity
}

pred init [s : State] {
    int s.capacity = 5000 and
    int s.contents = 0
}

assert initEstablishes {
    all s : State | init [s] implies invariant [s]
}

pred preFill [s : State, items: set Item] {
    #s.contents + #items <= #s.capacity
}

pred okFill [s, s' : State, amount : Int] {
    preFill [s, amount]
    int s'.contents = int s.contents + int amount
    int s'.capacity = int s.capacity
}

pred overFill [s, s' : State, amount : Int] {
    not preFill [s, amount]
    int s'.contents = int s.contents
    int s'.capacity = int s.capacity
}

pred fill [s, s' : State, amount : Int] {
    okFill [s, s', amount] or overFill [s, s', amount]
}

assert fillPreserves {
    all s, s' : State, amount : Int | invariant [s] and fill[s, s', amount] implies invariant [s']
}

pred remove [s, s' : State, amount : Int] {
    s.contents = s'.contents + amount
}

assert removeUndoesFill {
    all s, s', s'' : State, amount : Int |
       fill [s, s', amount] and remove [s', s'', amount] implies s.contents = s''.contents
}

assert fillComplete {
     all s : State, amount : Int |
          invariant [s]  and preFill[s, amount] implies 
          //some s' : State | fill [s, s', amount] and
          preFill [s,amount] or not preFill [s, amount]
}

pred fillc [s, s' : State, amount : Int] {
     invariant [s] 
     preFill[s, amount]
     not okFill [s, s', amount]
}

run fillc for 1 but 3 State

//check fillPreserves for 10 but 3 State
//check fillComplete for 10 but 3 State

//check removeUndoesFill for 1


/*
pred show [b : Book] {
	#b.addr > 1
   #Name.(b.addr) > 1
}

pred add [b, b': Book, n: Name, a: Addr] {
	b'.addr = b.addr + n->a
}

pred del [b, b': Book, n: Name] {
	b'.addr = b.addr - n->Addr
}

fun lookup [b: Book, n: Name] : set Addr {
	n.(b.addr)
}
*/
/*
pred showAdd [b, b': Book, n: Name, a: Addr] {
	add [b, b', n, a]
	#Name.(b'.addr) > 1
}
run showAdd for 3 but 2 Book
assert delUndoesAdd {
	all b, b', b'': Book, n : Name, a: Addr |
		no n.(b.addr) and add [b, b', n, a] and del [b', b'', n]
		implies
		b.addr = b''.addr
}
assert addIdempotent {
	all b, b', b'': Book, n: Name, a: Addr |
		add [b, b', n, a] and add [b', b'', n, a]
		implies
		b'.addr = b''.addr
}

assert addLocal {
	all b, b': Book, n, n': Name, a: Addr |
		add [b, b', n, a] and n != n'
		implies
		lookup [b, n'] = lookup [b', n']
}
// This command should not find any counterexample.
check delUndoesAdd for 20 but 3 Book

// This command should not find any counterexample.
check delUndoesAdd for 10 but 3 Book

// This command should not find any counterexample.
check addIdempotent for 3

// This command should not find any counterexample.
check addLocal for 3 but 2 Book
*/
