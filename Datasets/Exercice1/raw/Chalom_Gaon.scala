import scala.io.StdIn.readLine
import scala.util.Random

object Nospresso {
  var stockCafe = 50.0
  var stockSucre = 30.0
  var stockLait = 0.500
  
  val PIN = "434343"

  def main(args: Array[String]): Unit = {
    var continuer = true
    
    while (continuer) {
      println("\n        Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      
      var choix = 0
      while (choix < 1 || choix > 3) {
        choix = readLine(">").toIntOption.getOrElse(0)
      }
      
      if (choix == 1) {
        modeClient()
      } else if (choix == 2) {
        modeAdmin()
      } else if (choix == 3) {
        continuer = false
      }
    }
  }

  def modeClient(): Unit = {
    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    
    var prixBase = 0.0
    var prixSucre = 0.0
    var prixLaitSupp = 0.0
    var prixTotal = 0.0
    var cafeNecessaire = 0.0
    var laitNecessaire = 0.0
    var sucreNecessaire = 0.0
    var nomBoisson = ""
    
    var choixBoisson = 0
    while (choixBoisson < 1 || choixBoisson > 3) {
      choixBoisson = readLine(">").toIntOption.getOrElse(0)
    }
    
    var tailleLatte = 0
    if (choixBoisson == 3) {
      println("Choisissez la taille :")
      println("1) Petit")
      println("2) Moyen")
      println("3) Grand")
      while (tailleLatte < 1 || tailleLatte > 3) {
        tailleLatte = readLine(">").toIntOption.getOrElse(0)
      }
    }
    
    if (choixBoisson == 1) {
      cafeNecessaire = 8.0
      prixBase = 2.00
      prixTotal = prixBase
      nomBoisson = "Expresso"
      println("Boisson sélectionnée : Expresso")
    } else if (choixBoisson == 2) {
      cafeNecessaire = 6.0
      laitNecessaire = 0.100
      prixBase = 2.50
      prixTotal = prixBase
      nomBoisson = "Cappuccino"
      println("Boisson sélectionnée : Cappuccino")
    } else if (choixBoisson == 3) {
      nomBoisson = "Latte"
      if (tailleLatte == 1) {
        cafeNecessaire = 6.0
        laitNecessaire = 0.120
        prixBase = 2.70
        prixTotal = prixBase
        println("Boisson sélectionnée : Latte (Petit)")
      } else if (tailleLatte == 2) {
        cafeNecessaire = 8.0
        laitNecessaire = 0.150
        prixBase = 3.20
        prixTotal = prixBase
        println("Boisson sélectionnée : Latte (Moyen)")
      } else if (tailleLatte == 3) {
        cafeNecessaire = 12.0
        laitNecessaire = 0.200
        prixBase = 3.70
        prixTotal = prixBase
        println("Boisson sélectionnée : Latte (Grand)")
      }
    }

    if (stockCafe < cafeNecessaire) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return
    }
    
    if (stockLait < laitNecessaire) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      return
    }

    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    
    var choixSucre = 0
    while (choixSucre < 1 || choixSucre > 4) {
      choixSucre = readLine(">").toIntOption.getOrElse(0)
    }
    
    var niveauSucre = "Sans sucre"
    if (choixSucre == 2) {
      sucreNecessaire = 5.0
      prixSucre = 0.10
      prixTotal += prixSucre
      niveauSucre = "Peu (5g)"
    } else if (choixSucre == 3) {
      sucreNecessaire = 10.0
      prixSucre = 0.20
      prixTotal += prixSucre
      niveauSucre = "Moyen (10g)"
    } else if (choixSucre == 4) {
      sucreNecessaire = 15.0
      prixSucre = 0.30
      prixTotal += prixSucre
      niveauSucre = "Beaucoup (15g)"
    }
    println(s"Niveau de sucre : $niveauSucre")

    if (stockSucre < sucreNecessaire) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez choisir une autre option ou vérifier les stocks en mode Admin.")
      return
    }

    var laitSupplementaire = "Non"
    if (choixBoisson == 2 || choixBoisson == 3) {
      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non")
      
      var choixLait = 0
      while (choixLait < 1 || choixLait > 2) {
        choixLait = readLine(">").toIntOption.getOrElse(0)
      }
      
      if (choixLait == 1) {
        println("Combien de doses ?")
        var nbDoses = -1
        while (nbDoses < 0 || nbDoses > 3) {
          nbDoses = readLine(">").toIntOption.getOrElse(-1)
        }

        val laitSupp = 0.050 * nbDoses
        if (stockLait >= laitNecessaire + laitSupp) {
          laitNecessaire += laitSupp
          prixLaitSupp = 0.05 * nbDoses
          prixTotal += prixLaitSupp
          laitSupplementaire = "Oui"
        } else {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          return
        }
      }
    }
    println(s"Lait en supplément: $laitSupplementaire")

    if (prixSucre > 0 && prixLaitSupp > 0) {
      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", 
             prixBase, prixSucre, prixLaitSupp, prixTotal)
    } else if (prixSucre > 0) {
      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", 
             prixBase, prixSucre, prixTotal)
    } else if (prixLaitSupp > 0) {
      printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f\n", 
             prixBase, prixLaitSupp, prixTotal)
    } else {
      printf("Prix total : CHF %.2f\n", prixTotal)
    }
    
    val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
    println("Veuillez payer en utilisant Twint.")
    println(s"Votre code de paiement est : $codeTwint")
    println("(En attente de validation du paiement...)")
    
    Thread.sleep(3000)
    println("Paiement confirmé.")
    
    println("Préparation de votre boisson...")
    Thread.sleep(5000)
    
    stockCafe -= cafeNecessaire
    stockLait -= laitNecessaire
    stockSucre -= sucreNecessaire
    
    println(s"Votre $nomBoisson est prêt ! Bonne dégustation !")
  }

  def modeAdmin(): Unit = {
    println("Mode Admin")
    print("Entrez le code PIN : ")
    var pinEntre = ""
    while (pinEntre != PIN) {
      pinEntre = readLine(">")
    }
    
    println("Accès autorisé.")
    println("Stocks:")
    println(s"   Poudre de café: ${stockCafe}g")
    println(s"   Lait : ${stockLait}L")
    println(s"   Sucre : ${stockSucre}g")
    
    println("\nRéapprovisionnement des stocks...")
    println("Ajout :")
    
    var ajoutCafe = -1.0
    while (ajoutCafe < 0) {
      print("   Poudre de café: ")
      ajoutCafe = readLine(">").toDoubleOption.getOrElse(-1.0)
    }
    stockCafe += ajoutCafe
    
    var ajoutLait = -1.0
    while (ajoutLait < 0) {
      print("   Lait : ")
      ajoutLait = readLine(">").toDoubleOption.getOrElse(-1.0)
    }
    stockLait += ajoutLait
    
    var ajoutSucre = -1.0
    while (ajoutSucre < 0) {
      print("   Sucre : ")
      ajoutSucre = readLine(">").toDoubleOption.getOrElse(-1.0)
    }
    stockSucre += ajoutSucre
    
    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")
  }
}