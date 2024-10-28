package main

// um texto, de fato, pra ver se o compilador ta funcionando direito.
// espero que dê tudo certo, apesar dos pesares `code`
//
// Mais um parágrafo pra confirmar isso
type nome string

// teste pra uma TAD. Quero ver se vai
// direitinho
type data struct {
	field int
}

// constante
const idade = 10

// documentação da função.
// [https://github.com](Link)
func soma() int {
	return 2 + 2
}

func main() {
	print("hello world\n")
	print(soma())
}
