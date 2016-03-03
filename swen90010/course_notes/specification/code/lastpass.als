module LastPass

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

fact NoDuplicates 
{
    all pb : PassBook, user : Username, url : URL | lone pb.password[user][url]
}

//Add a password for a new user/url pair
pred add [pb, pb': PassBook, url : URL, user: Username, pwd: Password] {
//    no pb.password[user][url] 
    pb'.password = pb.password + (user->url->pwd)
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

//If we add a new password, then we get this password when we look it up
assert addWorks {
    all pb, pb': PassBook, url : URL, user : Username, p : Password |
        add [pb, pb', url, user, p] => (lookup [pb', url, user] = p)
}

//If we update an existing password, then we get this password when we look it up
assert updateWorks {
    all pb, pb': PassBook, url : URL, user : Username, p, p' : Password |
        lookup [pb', url, user] = p => 
            (add [pb, pb', url, user, p'] => (lookup [pb', url, user] = p'))
}

//If we add and then delete a password, we are back to 'the start'
assert deleteIsUndo {
    all pb1, pb2, pb3: PassBook, url : URL, user : Username, pwd : Password |
        add [pb1, pb2, url, user, pwd] && delete [pb2, pb3, url, user]
            => pb1.password = pb3.password
}

run add for 3 but 2 PassBook
run add for 5 URL, 5 Username, 10 Password, 2 PassBook
check addWorks for 3 but 2 PassBook expect 0
check updateWorks for 3 but 2 PassBook expect 0
check deleteIsUndo for 3 but 2 PassBook
