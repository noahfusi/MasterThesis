import io.StdIn._

var pin = 434343
var poudrecafe = 50
var sucre = 30
var lait = 500
var running = true

while (running) {
    println("Veuillez sélectionner votre mode :")
    println("1) Client")
    println("2) Admin")
    println("3) Quitter")
    print("> ")
    val choix = readLine()

    if (choix == "1") {
        var prix = 0.00 
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        val choixBoisson = readLine()

        if (choixBoisson == "1") {
            if (poudrecafe >= 8) {
                poudrecafe -= 8
                prix += 2.00
                println("Expresso sélectionné. CHF 2.00")
            } else {
                println("Quantité insuffisante de poudre de café.")
            }
        } else if (choixBoisson == "2") {
            if (poudrecafe >= 6 && lait >= 100) {
                poudrecafe -= 6
                lait -= 100
                prix += 2.50
                println("Cappuccino sélectionné. CHF 2.50")
            } else {
                println("Ingrédients insuffisants.")
            }
        } else if (choixBoisson == "3") {
            println("Sélectionnez la taille de votre Latte :")
            println("1) Petit (CHF 2.70)")
            println("2) Moyen (CHF 3.20)")
            println("3) Grand (CHF 3.70)")
            print("> ")
            val tailleLatte = readLine()

            if (tailleLatte == "1" && poudrecafe >= 6 && lait >= 120) {
                poudrecafe -= 6
                lait -= 120
                prix += 2.70
                println("Latte Petit sélectionné. CHF 2.70")
            } else if (tailleLatte == "2" && poudrecafe >= 8 && lait >= 150) {
                poudrecafe -= 8
                lait -= 150
                prix += 3.20
                println("Latte Moyen sélectionné. CHF 3.20")
            } else if (tailleLatte == "3" && poudrecafe >= 12 && lait >= 200) {
                poudrecafe -= 12
                lait -= 200
                prix += 3.70
                println("Latte Grand sélectionné. CHF 3.70")
            } else {
                println("Ingrédients insuffisants pour la taille choisie.")
            }
        } else {
            println("Pas un nombre valide.")
        }

        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print("> ")
        val sugar = readLine().toInt
        if (sugar == 1) {
 
        } else if (sugar == 2 && sucre >= 5) {
            sucre -= 5
            prix += 0.10
        } else if (sugar == 3 && sucre >= 10) {
            sucre -= 10
            prix += 0.20
        } else if (sugar == 4 && sucre >= 15) {
            sucre -= 15
            prix += 0.30
        } else {
            println("Sucre insuffisant/choix invalide.")
        }

        if (prix > 0) {
            println(s"Prix total: CHF $prix")
            println("Veuillez payer en utilisant Twint.")
            println("Votre code de paiement est : AB12X")
            println("(En attente de validation du paiement...)")  
            Thread.sleep(5000)
            println("Merci ! Votre paiement a été accepté.")  
            println("Préparation de votre boisson...")
            Thread.sleep(5000)
            println("[...]")

            if (choixBoisson == "1") {
                println("Votre Expresso est prêt. Bonne dégustation !")
            } else if (choixBoisson == "2") {
                println("Votre Cappuccino est prêt. Bonne dégustation !")
            } else if (choixBoisson == "3") {
                println("Votre Latte est prêt. Bonne dégustation !")
            }
        }
    } else if (choix == "2") {
        print("Entrez le code PIN : ")
        val saisiePin = readLine()
        if (saisiePin == pin.toString) {
            println("Mode Admin activé.")
            println(s"Stocks actuels : Poudre de café = $poudrecafe g, Sucre = $sucre g, Lait = ${lait / 1000.0} L.")
            println("Entrez les quantités à ajouter (en grammes/ml) :")
            print("Poudre de café : ")
            poudrecafe += readLine().toInt
            print("Sucre : ")
            sucre += readLine().toInt
            print("Lait : ")
            lait += readLine().toInt
            println("Stocks mis à jour.")
        } else {
            println("Code PIN invalide.")
        }
    } else if (choix == "3") {
        running = false
    } else {
        println("Pas un choix valide.")
    }
}



