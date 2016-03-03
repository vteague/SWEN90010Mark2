module ExtendedLastPass

open util/ordering[PassBook] as ord

/*
 * LastPass password map
 *
 * A simple example to explain basics of Alloy. 
 *
 * The 'PassBook' keeps track of a set of users' passwords for a set of URLs. 
 * For each User/URL pair, there is one password. Passwords can be added, 
 * deleted, looked up, and changed. 
 * A user can also request all of their password for all URLs.
 *
 * author: Tim Miller
 */

sig URL {}
sig Username {}
sig Password {}
sig PassBook {known : Username -> URL, password : known -> one Password}

//Report signatures for informing whether an opertion was successful or not
abstract sig Report {}
sig Failed extends Report {}
sig Success extends Report {}

//Each user/url pair maps to exactly one password
pred inv [pb : PassBook] {
    all user : Username, url : URL | lone pb.password[user][url]
}

//True if and only if the user has no password for this URL
pred preAdd [pb: PassBook, url : URL, user: Username] {
    no pb.password[user][url] 
}

//Add a password for a user/url pair
pred postAdd [pb, pb': PassBook, url : URL, user: Username, pwd: Password] {
    pb'.password = pb.password + (user->url->pwd)
}

//Add a password for a *new* user/url pair
pred addOk [pb, pb': PassBook, url : URL, user: Username, 
                     pwd: Password, report : one Report] {
    preAdd [pb, url, user]
    postAdd [pb, pb', url, user, pwd] 
    report in Success
}

//Fail to add a password that already exists
pred addDuplicate [pb, pb': PassBook, url : URL, user: Username, 
                                report : one Report] {
    not preAdd [pb, url, user]
    pb = pb'
    report in Failed
}

//Add a password for a *new* user/url, otherwise, add nothing
pred add [pb, pb': PassBook, url : URL, user: Username, pwd: Password, 
                 report : one Report] {
    addOk [pb, pb', url, user, pwd, report] 
      or 
    addDuplicate [pb, pb', url, user, report]
}

//Delete an existing password
pred delete [pb, pb': PassBook, url : URL, user: Username] {
    one pb.password[user][url]
    pb'.password = pb.password - (user->url->Password)
}

//Update an existing password
pred update [pb, pb': PassBook, url : URL, user: Username, pwd: Password] {
    one pb.password[user][url] 
    pb'.password = pb.password ++ (user->url->pwd)
}

//Return the password for a given user/URL pair
fun lookup [pb: PassBook, url : URL, user : Username] : lone Password {
    pb.password[user][url]
}

//Check if a user's passwords for two urls are the same
pred samePassword [pb : PassBook, url1, url2 : URL, user : Username] {
    lookup [pb, url1, user] = lookup [pb, url2, user]
}

//Retrieve all of the passwords and the url they are for, for a given user
pred retrieveAll [pb: PassBook, user : Username, pwds : URL -> Password] {
    pwds = (pb.password)[user]
}

//Initialise the PassBook to be empty
pred init [pb: PassBook] {
    no pb.known
}

assert initEstablishes {
    all pb : PassBook | init [pb] => inv [pb]
}

assert addPreservesInv {
    all pb, pb' : PassBook, user : Username, url : URL, 
        pwd : Password, report : Report | 
        inv [pb] and add [pb, pb', url, user, pwd, report] => inv [pb']
}

assert updatePreservesInv {
    all pb, pb' : PassBook, user : Username, url : URL, pwd : Password | 
        inv [pb] and update [pb, pb', url, user, pwd] => inv [pb']
}

assert deletePreservesInv {
    all pb, pb' : PassBook, user : Username, url : URL | 
        inv [pb] and delete [pb, pb', url, user] => inv [pb']
}

//If we add a new password, then we get this password when we look it up
assert addWorks {
    all pb, pb': PassBook, url : URL, user : Username, p : Password, report : Report |
        addOk [pb, pb', url, user, p, report] => 
            (lookup [pb', url, user] = p and report in Success)
}

//If we update an existing password, then we get this password when we look it up
assert updateWorks {
    all pb, pb': PassBook, url : URL, user : Username, p, p' : Password |
        lookup [pb, url, user] = p => 
            (update [pb, pb', url, user, p'] => (lookup [pb', url, user] = p'))
}

//If we add and then delete a password, we are back to 'the start'
assert deleteIsUndo {
    all pb1, pb2, pb3: PassBook, url : URL, user : Username, pwd : Password, report : Report |
        addOk [pb1, pb2, url, user, pwd, report] && delete [pb2, pb3, url, user]
            => pb1.password = pb3.password
}

//If we delete something that is not in the book, nothing happens
assert deleteDuplicateDoesNothing {
   all pb, pb' : PassBook, url : URL, user : Username |
      no lookup [pb, url, user] =>
          (delete [pb, pb', url, user] => pb = pb')
}

run add for 3 but 2 PassBook
run add for 5 URL, 5 Username, 10 Password, 2 Report, 1 Failed, 1 Success, 2 PassBook
check initEstablishes for 3 but 1 PassBook expect 0 
check addPreservesInv for 3 but 2 PassBook expect 0
check updatePreservesInv for 3 but 2 PassBook expect 0
check deletePreservesInv for 3 but 2 PassBook expect 0
check addWorks for 3 but 2 PassBook expect 0
check updateWorks for 3 but 2 PassBook expect 0
check deleteIsUndo for 3 but 2 PassBook expect 0
check deleteDuplicateDoesNothing for 3 but 2 PassBook expect 0
