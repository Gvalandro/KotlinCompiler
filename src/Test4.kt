fun main() {
    val a: Int 
    val b: Int
    val c: Boolean

    a = 15
    b = 10
    c = ((a > b) && (a != b)) || !(a < b)

    if (c) {
        print(c)
    } else {
        print(0)
    }
}
