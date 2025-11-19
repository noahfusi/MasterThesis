import scala.io.StdIn
import scala.util.Random

var cafe = 50
var sucre = 30
var lait = 0.5
var prix = 0.0
val code = 5

def Twint(): String = Random.alphanumeric.take(code).mkString

while (true) {
  println("Nespresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n")
  print("> ")
  var mode: Byte = StdIn.readLine().toByte

  if (mode == 1) {
    while (true) {
      println("Veuillez sélectionner votre produit :\n1) Expresso\n2) Cappuccino\n3) Latte\n4) Retour au menu principal")
      print("> ")
      var produit: Byte = StdIn.readLine().toByte

      if (produit == 1) {
        println("Souhaitez-vous ajouter du sucre ? :\n1) Pas de sucre\n2) Peu de sucre (5g)\n3) Quantité moyenne (10g)\n4) Beaucoup de sucre (15g)\n5) Retour")
        var quantiteeS: Byte = StdIn.readLine().toByte

        if (quantiteeS == 1) {
          if (cafe >= 8) {
            cafe = cafe - 8
            prix = 2.0
            println("Prix à payer: 2 CHF")
          } else {
            println("Produit momentanément indisponible.")
          }
        } else if (quantiteeS == 2) {
          if (cafe >= 8 && sucre >= 5) {
            cafe = cafe - 8
            sucre = sucre - 5
            println("Prix à payer: 2.10 CHF")
          } else {
            println("Produit momentanément indisponible.")
          }
        } else if (quantiteeS == 3) {
          if (cafe >= 8 && sucre >= 10) {
            cafe = cafe - 8
            sucre = sucre - 10
            println("Prix à payer: 2.20 CHF")
          } else {
            println("Produit momentanément indisponible.")
          }
        } else if (quantiteeS == 4) {
          if (cafe >= 8 && sucre >= 15) {
            cafe = cafe - 8
            sucre = sucre - 15
            println("Prix à payer: 2.30 CHF")
          } else {
            println("Produit momentanément indisponible.")
          }
        } else if (quantiteeS == 5) {
        } else {
          println("Produit momentanément indisponible")
        }

      } else if (produit == 2) {
        println("Souhaitez-vous ajouter du sucre ? :\n1) Pas de sucre\n2) Peu de sucre (5g)\n3) Quantité moyenne (10g)\n4) Beaucoup de sucre (15g)\n5) Retour")
        var quantiteeS: Byte = StdIn.readLine().toByte

        if (quantiteeS == 1) {
          if (cafe >= 6 && lait >= 0.1) {
            cafe = cafe - 6
            lait = lait - 0.1
            prix = 2.50
            println("Prix à payer: 2.50 CHF")
          } else {
            println("Produit momentanément indisponible.")
          }
        } else if (quantiteeS == 2) {
          if (cafe >= 6 && lait >= 0.1 && sucre >= 5) {
            cafe = cafe - 6
            lait = lait - 0.1
            sucre = sucre - 5
            prix = 2.60
            println("Prix à payer: 2.60 CHF")
          } else {
            println("Produit momentanément indisponible.")
          }
        } else if (quantiteeS == 3) {
          if (cafe >= 6 && lait >= 0.1 && sucre >= 10) {
            cafe = cafe - 6
            lait = lait - 0.1
            sucre = sucre - 10
            prix = 2.70
            println("Prix à payer: 2.70 CHF")
          } else {
            println("Produit momentanément indisponible.")
          }
        } else if (quantiteeS == 4) {
          if (cafe >= 6 && lait >= 0.1 && sucre >= 15) {
            cafe = cafe - 6
            lait = lait - 0.1
            sucre = sucre - 15
            prix = 2.80
            println("Prix à payer: 2.80 CHF")
          } else {
            println("Produit momentanément indisponible.")
          }
        } else if (quantiteeS == 5) {
        } else {
          println("Veuillez entrer une valeur valide")
        }

      } else if (produit == 3) {
        println("Souhaitez-vous un café ? :\n1) Petit\n2) Moyen\n3) Grand\n4) Retour")
        var mode1: Byte = StdIn.readLine().toByte

        var Latte = true
        while (Latte) {
          if (mode1 == 1) {
            if (cafe >= 6 && lait >= 0.12) {
              cafe = cafe - 6
              lait = lait - 0.12
              prix = 2.70
              println("Prix à payer: " + prix + " CHF")
              Latte = false
            } else {
              println("Produit momentanément indisponible.")
            }
          } else if (mode1 == 2) {
            if (cafe >= 8 && lait >= 0.15) {
              cafe = cafe - 8
              lait = lait - 0.15
              prix = 3.20
              println("Prix à payer: " + prix + " CHF")
              Latte = false
            } else {
              println("Produit momentanément indisponible.")
            }
          } else if (mode1 == 3) {
            if (cafe >= 12 && lait >= 0.2) {
              cafe = cafe - 12
              lait = lait - 0.2
              prix = 3.70
              println("Prix à payer: " + prix + " CHF")
              Latte = false
            } else {
              println("Produit momentanément indisponible.")
            }
          } else {
            println("Veuillez entrer une valeur valide")
          }
        }

        println("Souhaitez-vous ajouter du sucre ? :\n1) Pas de sucre\n2) Peu de sucre (5g)\n3) Quantité moyenne (10g)\n4) Beaucoup de sucre (15g)\n5) Retour")
        var mode2: Byte = StdIn.readLine().toByte

        var Sucre1 = true
        while (Sucre1) {
          if (mode2 == 1) {
            println("Prix à payer: " + prix + " CHF")
            Sucre1 = false
          } else if (mode2 == 2) {
            if (sucre >= 5) {
              sucre = sucre - 5
              prix = prix + 0.1
              println("Prix à payer: " + prix + " CHF")
              Sucre1 = false
            } else {
              println("Produit momentanément indisponible.")
            }
          } else if (mode2 == 3) {
            if (sucre >= 10) {
              sucre = sucre - 10
              prix = prix + 0.2
              println("Prix à payer: " + prix + " CHF")
              Sucre1 = false
            } else {
              println("Produit momentanément indisponible.")
            }
          } else if (mode2 == 4) {
            if (sucre >= 15) {
              sucre = sucre - 15
              prix = prix + 0.3
              println("Prix à payer: " + prix + " CHF")
              Sucre1 = false
            } else {
              println("Produit momentanément indisponible.")
            }
          } else if (mode2 == 5) {
            Sucre1 = false
          } else {
            println("Veuillez entrer une valeur valide")
          }
        }
      }

      if (produit >= 1 && produit <= 3) {
        val Twint = Twint()
        println(s"Code Twint généré : " + Twint)
        println("Paiement en cours...")
        Thread.sleep(3000)
        println("Paiement validé. Merci !")
      } else if (produit == 4) {
      } else {
        println("Veuillez entrer une valeur valide")
      }
    }
  } else if (mode == 2) {
    println("Mode Administrateur\nVeuillez entrer le code PIN : ******")
    print("> ")
    var pin: Short = StdIn.readLine().toShort

    while (pin != 434343) {
      println("Code erroné. Veuillez réessayer.")
      pin = StdIn.readLine().toShort
    }

    println("Accès autorisé.")
    println("Poudre de café : " + cafe + "g")
    println("Lait : " + lait + "L")
    println("Sucre : " + sucre + "g")

    println("Souhaitez-vous réapprovisionner les Stocks?\n1) Oui\n2) Non")
    var approvisionnement: Byte = StdIn.readLine().toByte
    if (approvisionnement == 1) {
      println("Souhaitez-vous réapprovisionner :\n1) Poudre de café\n2) Lait\n3) Sucre")
      var produit: Byte = StdIn.readLine().toByte

      if (produit == 1) {
        println("Indiquez la quantité en grammes de l'apport")
        var apportP: Short = StdIn.readLine().toShort
        cafe = cafe + apportP
      } else if (produit == 2) {
        println("Indiquez la quantité en litres de l'apport")
        var apportL: Double = StdIn.readLine().toDouble
        lait = lait + apportL
      } else if (produit == 3) {
        println("Indiquez la quantité en grammes de l'apport")
        var apportS: Short = StdIn.readLine().toShort
        sucre = sucre + apportS
      } else {
        println("Veuillez entrer une valeur valide")
      }
    }
  } else if (mode == 3) {
    println("À bientôt")
    System.exit(0)
  } else {
    println("Veuillez entrer une valeur valide")
  }
}