
import scala.io.StdIn._
import scala.util.Random

object Main {
  val nbMachine = 5
  var stockCafe = Array.fill(nbMachine+1)(50.0)
  var stockSucre = Array.fill(nbMachine+1)(30.0)
  var stockLait = Array.fill(nbMachine+1)(0.5)
  var machinePins = Array.fill(nbMachine+1)("434343")

  def main(args: Array[String]): Unit = {
    var continuer = true
    while (continuer) {
      println("Bienvenue dans NosprossoCafé  ")
      println("Veuillez sélectionner une machine de (1 à " + nbMachine + ") ou -1 pour quitter : ")
      var machineId = -2

      while (machineId < -1 || machineId > nbMachine) {
        machineId = readInt()
        if (machineId < -1 || machineId > nbMachine) {
          println("Veuillez sélectionner une machine de (1 à " + nbMachine + ") ou -1 pour quitter : ")
        }
      }
      if (machineId == -1) {
        println("Programme terminé. Au revoir !")
        continuer = false
      } else {
        val machineIndex = machineId - 1

        println("Veuillez sélectionner votre mode : ")
        println("1- Client ")
        println("2- Admin ")
        println("3- Retour au menu principal")
        println("4- Quitter")

        var Choix = 0
        while (Choix != 1 && Choix != 2 && Choix != 3 && Choix != 4) {
          Choix = readInt()
          if (Choix != 1 && Choix != 2 && Choix != 3 && Choix != 4) {
            println(" Veuillez entrer un choix Valide (1,2,3,4). ")
          }
        }
        if (Choix == 1) {
          if (!serveClient(machineId, stockCafe, stockSucre, stockLait)) {
            println("Transaction échouée. Veuillez vérifier les stocks ou essayer une autre machine.")
          }
        } else if (Choix == 2) {
          if (validePin(machineIndex, machinePins)) {
            println("1- Réapprovisonner ")
            println("2- Modifier le PIN")
            var action = -1
            while (action != 1 && action != 2) {
              action = readInt()
              action = readInt()
              if (action != 1 && action != 2) {
                println("Veuillez entrer un choix valide (1 ou 2).")
              }
            }
            if (action == 1) {
              restockMachine(machineIndex, stockCafe, stockSucre, stockLait)
            } else if (action == 2) {
              updatePin(machineIndex, machinePins)
            }
          }
        }
      }
    }
  }
  def validePin(machineIndex: Int, machinePins : Array[String]): Boolean = {
    var essais = 3
    var pinCorrect = false

    while (essais > 0 && !pinCorrect) {
      println("Entrez le code PIN pour la machine " + (machineIndex+1)+ ":")
      val pinEntree = readLine()

      if (pinEntree == machinePins(machineIndex)) {
        println("Code PIN valide. Accès accordé.")
        pinCorrect = true
      } else {
        essais -= 1
        if (essais > 0) {
          println("Code incorrect." + essais + "tentative(s) restante(s).")
        } else {
          println ("Code PIN incorrect. 0 tentative restante .")
          println("Trop de tentatives échouées.")

        }
      }
    }
    pinCorrect
  }

  def updatePin(machineIndex: Int,machinePins : Array[String]): Unit = {
    println("Mise à jour du code PIN pour la machine " + machineIndex+1 + ".")
    var pinValide = false

    while (!pinValide) {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      val nouveauPin = readLine()

      if (nouveauPin.length == 6) {
        var valide = true


        for (i <- 0 until nouveauPin.length) {
          if (nouveauPin(i) < '0' || nouveauPin(i) > '9') {
            valide = false
          }
        }
        if (valide) {
          machinePins(machineIndex) = nouveauPin
          pinValide = true
          println("Code PIN mis à jour avec succès.")
          println("Retour au menu principal ")
        } else {
          println("Erreur : le code PIN doit contenir uniquement des chiffres.")
        }
      } else {
        println("Erreur : le code PIN doit comporter exactement 6 caractères.")
      }
    }
  }

  def serveClient(machineIndex: Int,stockCafe : Array[Double],stockSucre : Array[Double],stockLait : Array[Double]): Boolean = {


    println("Bienvenue dans NosprossoCafé - Machine " + machineIndex + "!")
    println("Veuillez sélectionner votre boisson :")
    println(" 1-Expresso : 2.00 CHF ")
    println(" 2-Cappucino : 2.50 CHF ")
    println(" 3-Latte : \n-Petit: 2.70 CHF,\n-Moyen : 3.20 CHF,\n-Grand : 3.70 CHF ")

    var ChoixBoisson = 0
    var prixBoisson = 0.0
    var besoinCafe = 0.0
    var besoinLait = 0.0
    var besoinSucre = 0.0
    var Boisson = ""


    while (ChoixBoisson < 1 || ChoixBoisson > 3) {
      ChoixBoisson = readInt()
      if (ChoixBoisson < 1 || ChoixBoisson > 3) {
        println("Veuillez entrer votre choix valide (1,2,3).")
      }
    }
    if (ChoixBoisson == 1) {
      Boisson = "Expresso"
      prixBoisson = 2.00
      besoinCafe = 8.0
    } else if (ChoixBoisson == 2) {
      Boisson = "Cappuccino"
      prixBoisson = 2.50
      besoinCafe = 6.0
      besoinLait = 0.1
    } else if (ChoixBoisson == 3) {
      println("Sélectionnez la taille de votre Latte : 1-Petit, 2-Moyen, 3-Grand")
      var taille = 0
      while ((taille < 1) || (taille > 3)) {
        taille = readInt()
        if ((taille < 1) || (taille > 3)) {
          println("Taille invalide. Veuillez entre 1,2 ou 3.")
        }
      }
      if (taille == 1) {
        Boisson = "Latte Petit."
        prixBoisson = 2.70
        besoinCafe = 6.0
        besoinLait = 0.120
      } else if (taille == 2) {
        Boisson = "Latte Moyen"
        prixBoisson = 3.20
        besoinCafe = 8.0
        besoinLait = 0.150
      } else if (taille == 3) {
        Boisson = "Latte Grand"
        prixBoisson = 3.70
        besoinCafe = 12.0
        besoinLait = 0.2
      }
    }

    println("Souhaitez vous ajouter du sucre ? ")
    println(" 1- Sans sucre.")
    println(" 2- Peu (5g) à 0.10 CHF.")
    println(" 3- Moyen (10g) à 0.20 CHF.")
    println(" 4- Beaucoup (15g) à 0.30 CHF.")

    var ChoixSucre = 0
    var prixSucre = 0.0

    while ((ChoixSucre < 1) || (ChoixSucre > 4)) {
      ChoixSucre = readInt()
      if ((ChoixSucre < 1) || (ChoixSucre > 4)) {
        println("Veuillez entrer un choix valide (1,2,3 ou 4).")
      }
    }

    if (ChoixSucre == 2) {
      prixSucre = 0.10
      besoinSucre = 5.0
    } else if (ChoixSucre == 3) {
      prixSucre = 0.20
      besoinSucre = 10.0
    } else if (ChoixSucre == 4) {
      prixSucre = 0.30
      besoinSucre = 15.0
    }

    var DoseLait = 0
    var prixLait = 0.0
    var ajouterLait = 2
    if ((ChoixBoisson == 2) || (ChoixBoisson == 3)) {
      println("Souhaitez-vous ajouter du lait supplément ?")
      print(" 1-oui ")
      print(" 2-Non ")
      ajouterLait = readInt()
      if (ajouterLait == 1) {
        println("Combien de doses ? : Max 3 doses")
        DoseLait = 0
        while ((DoseLait < 1) || (DoseLait > 3)) {
          DoseLait = readInt()
          if ((DoseLait < 1) || (DoseLait > 3)) {
            println("Erreur : Le nombre de doses doit etre entre 1 et 3.")
          }
        }
        prixLait = DoseLait * 0.05
        besoinLait += DoseLait * 0.05
      }
    }

    val prixTotal = prixBoisson + prixSucre + prixLait
    var stockSuffisant = false
    while (!stockSuffisant) {
      if (besoinCafe > stockCafe(machineIndex)) {
        println("Erreur : Quantité de poudre de café insuffisante.")
      } else if (besoinSucre > stockSucre(machineIndex)) {
        println("Erreur : Quantité de sucre insuffisante.")
      } else if (besoinLait > stockLait(machineIndex)) {
        println("Erreur : Quantité de lait insuffisante.")
      } else {
        stockCafe(machineIndex) -= besoinCafe
        stockSucre(machineIndex) -=besoinSucre
        stockLait(machineIndex) -=besoinLait
        println ("Votre boisson est en préparation. Merci de votre achat !")
        stockSuffisant = true
      }

      if (!stockSuffisant) {
        println("Que voulez-vous faire ?")
        println("1- Modifier votre commande")
        println("2- Retourner au menu principal")
        val choix = readInt()
        if (choix == 2) return false
        else return serveClient(machineIndex, stockCafe, stockSucre, stockLait)
      }
    }


    println("Récapitulatif  de votre boisson")
    println("Boisson : " + Boisson)
    if (ChoixSucre == 1) {
      println(" Sans sucre ")
    } else if (ChoixSucre == 2) {
      println(" Peu de sucre (5g).")
    } else if (ChoixSucre == 3) {
      println(" Moyen de sucre (10g).")
    } else {
      println("Beaucoup de sucre (15g).")
    }
    if (DoseLait > 0) {
      println("Lait en supplément : " + DoseLait + " doses")
    } else {
      println("Pas de lait en supplément.")
    }
    if ((ChoixSucre == 1) && (ajouterLait == 2)) {
      printf("Prix Total : CHF %.2f\n", prixBoisson)
    } else if (((ChoixSucre == 2) || (ChoixSucre == 3) || (ChoixSucre == 4)) && (ajouterLait == 2)) {
      printf("Prix Total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixBoisson, prixSucre, prixTotal)
    } else if ((ChoixSucre == 1) && (ajouterLait == 1)) {
      printf("Prix Total : CHF %.2f + CHF %.2f = CHF %.2f\n", prixBoisson, prixLait, prixTotal)
    } else if (((ChoixSucre == 2) || (ChoixSucre == 3) || (ChoixSucre == 4)) && (ajouterLait == 1)) {
      printf("Prix Total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixBoisson, prixLait,prixSucre, prixTotal)
    }

    println("Veuillez payer en utilisant Twint.")
    val codeTwint = Random.alphanumeric.take(5).mkString
    println("Votre code de paiment est :" + codeTwint)
    println("(En attente de paiement...)")
    Thread.sleep(5000)
    println("Paiement confirmé.")
    println("Préparation de votre boisson...")
    println("votre " + Boisson + " est pret ! Bonne dégustation !")

    true
  }

  def restockMachine(machineIndex: Int,stockCafe: Array[Double],stockSucre : Array[Double],stockLait : Array[Double]): Unit = {
    println("Réapprovisionnement de la machine " + machineIndex + ".")
    println("Stocks actuels : ")
    printf("Poudre de café : %.2f g\n ",stockCafe(machineIndex))
    printf("Sucre : %.2f g\n+", stockSucre(machineIndex))
    printf("Lait : %.2f l\n", stockLait(machineIndex))
    println("Entrez la quantité à ajouter pour chaque ingrédient :")


    var ajoutCafe = -1.0
    while (ajoutCafe < 0) {
      println("Poudre de café (en g) :")
      ajoutCafe = readDouble()
      if (ajoutCafe < 0) {
        println("Erreur : la quantité doit etre positive ")
      }
    }
    var ajoutSucre = -1.0
    while (ajoutSucre < 0) {
      println("Sucre (en g) :")
      ajoutSucre = readDouble()
      if (ajoutSucre < 0) {
        println("Erreur : la quantité doit etre positive ")
      }
    }
    var ajoutLait = -1.0
    while (ajoutLait < 0) {
      println("Lait (en L) :")
      ajoutLait = readDouble()
      if (ajoutLait < 0) {
        println("Erreur : la quantité doit etre positive ")
      }
    }

    stockCafe(machineIndex) += ajoutCafe
    stockSucre(machineIndex) += ajoutSucre
    stockLait(machineIndex) += ajoutLait

    println("Niveau de stocks mis à jour avec succès.")
    printf("Poudre de café : %.2f g\n", stockCafe(machineIndex))
    printf("Sucre : %.2f g\n", stockSucre(machineIndex))
    printf("Lait : %.2f l\n", stockLait(machineIndex))
    println("Retour au menu principal...")
  }
}







