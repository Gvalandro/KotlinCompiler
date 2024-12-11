fun main() {
    val a: Int
    val b: Int
    val c: Int 
    val d: Boolean 
    val e: Boolean
	
    // Testando operadores de comparação
    val comparison1 : Boolean
    val comparison2 : Boolean
    val comparison3 : Boolean
    val comparison4 : Boolean
    val comparison5 : Boolean
    val comparison6 : Boolean


    a = 10
    b = 5
    c = 8
    d = true
    e = false

    comparison1 = (a > b)
    comparison2 = (a <= c)
    comparison3 = (b == c)
    comparison4 = (a != b)
    comparison5 = (c >= a)
    comparison6 = (b < a)

    // Estrutura condicional com combinações de operadores lógicos e de comparação
    if (((a > b) && d) || ((c <= a) && (!e))) {
        print(a)
    } else {
        print(b)
    }
}
