import io.StdIn._
import scala.util.Random

object Main {

  // Etape 1: déclaration des méthodes
  // 1.1) méthode pour le code PIN du mode Admin: limitation à 3 tentatives (boucle for)
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {

    val tentatives = 3
    for (i <- 1 to tentatives) {
      println("Entrez le code PIN : ")
      println(">")
      val entreecodePin = readLine()

      if (entreecodePin == machinePins(machineId)) {
        println("Accès accordé à la machine " + (machineId + 1))
        return true
      }
      else {
        println("Code PIN incorrect. " + (tentatives - i) + " tentatives restantes.")
      }
    }
    println()
    println("Trop de tentatives échouées. Fin du programme.")
    return false
  }

  // 1.2) Méthode pour la modification du code PIN en mode Admin et selon la machine séléctionnée
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {

    println("Mise à jour du code PIN pour la machine " + (machineId + 1))
    println("Veuillez entrer un nouveau code PIN à 6 chiffres")
    println(">")
    var nouveaucodePin = readLine()

    // verification que l'entrée est valide (pour le code pin)
    while (nouveaucodePin.length != 6 || !nouveaucodePin.forall(_.isDigit)) {
      println("Nouveau code PIN invalide. Veuillez entrer un nouveau code PIN à 6 chiffres")
      println(">")
      nouveaucodePin = readLine()
    }
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
    machinePins(machineId) = nouveaucodePin
  }

  // 1.3) Méthode pour le mode client
  def serveClient(machineId: Int, stockcafe: Array[Int], stocksucre: Array[Int], stocklait: Array[Int]): Boolean = {

    // variables pour les boisons et la personalisation
    var choixboisson = 0.0
    var choixsucre = 0
    var choixlait = 0
    var choixdoselait = 0
    var boissonselectionnee1 = "" // vairable pour le récapitulatif (pour le latte ajout entre parenthèses de la taille)
    var boissonselectionnee = "" // variable pour la phrase après paiement (pour le latte, pas de précision de la taille)
    var sucreselectionne = ""
    var laitselectionne = ""

    // variables pour les prix
    var prixboisson = 0.00
    var prixtotal = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00

    // variables pour la gestion des stock
    var caferequis = 0
    var sucrerequis = 0
    var laitrequis = 0
    var stocksuffisant = 1

    // variables pour le code Twint
    val longeurcodeTwint = 5
    val alphanumeric = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    var codeTwint = ""

    // réinitialisation des prix (à 0) pour chaque transaction
    prixboisson = 0.0
    prixlait = 0.0
    prixsucre = 0.0
    stocksuffisant = 1

    // Choix de la boisson
    println("Veuillez sélectionner votre boisson:")
    println("1) Expresso - CHF 2.00")
    println("2) Capuccino - CHF 2.50")
    println("Latte:  3.1) Petit - CHF 2.70    3.2) Moyen - CHF 3.20    3.3) Grand - CHF 3.70")
    println(">")
    choixboisson = readDouble()

    // Vérification que l'entrée est valide (pour le choix de la boisson)
    while (choixboisson != 1 && choixboisson != 2 && choixboisson != 3.1 && choixboisson != 3.2 && choixboisson != 3.3) {
      println("Veuillez sélectionner une entrée valide")
      println(">")
      choixboisson = readDouble()
    }

    // changement des variables caferequis et laitrequis pour faciliter le calcul des stock
    // changement des variables boissonselectionnee et prixboisson pour faciliter l'affichage du recapitulatif
    // changement de la variable prixboisson pour faciliter le calcul du prix
    if (choixboisson == 1) {
      caferequis = 8
      laitrequis = 0
      boissonselectionnee1 = "Expresso"
      boissonselectionnee = boissonselectionnee1
      prixboisson = 2.00
    }
    else if (choixboisson == 2) {
      caferequis = 6
      laitrequis = 100
      boissonselectionnee1 = "Capuccino"
      boissonselectionnee = boissonselectionnee1
      prixboisson = 2.50
    }
    else if (choixboisson == 3.1) {
      caferequis = 6
      laitrequis = 120
      boissonselectionnee1 = "Latte (Petit)"
      boissonselectionnee = "Latte"
      prixboisson = 2.70
    }
    else if (choixboisson == 3.2) {
      caferequis = 8
      laitrequis = 150
      boissonselectionnee1 = "Latte (Moyen)"
      boissonselectionnee = "Latte"
      prixboisson = 3.20
    }
    else {
      caferequis = 12
      laitrequis = 200
      boissonselectionnee1 = "Latte (Grand)"
      boissonselectionnee = "Latte"
      prixboisson = 3.70
    }

    // Personalisation de la boisson: choix du sucre
    println("Souhaitez vous ajouter du sucre?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    println(">")
    choixsucre = readInt()

    // vérification que l'entrée est valide (pour le choix du sucre)
    while (choixsucre != 1 && choixsucre != 2 && choixsucre != 3 && choixsucre != 4) {
      println("Veuillez sélectionner une entrée valide")
      println(">")
      choixsucre = readInt()
    }

    // changement de la variable sucrerequis pour faciliter le calcul des stock
    // changement des variables sucreselectionne pour faciliter l'affichage du recapitulatif
    // changement de la variable prix pour faciliter le calcul du prix
    sucrerequis = 5 * (choixsucre - 1) // formule génlérale qui englobe les 3 cas possibles (permet de simplifier et reduire les instructions)
    prixsucre = 0.10 * (choixsucre - 1)

    if (choixsucre == 1) {
      sucreselectionne = "Sans sucre"
    }
    else if (choixsucre == 2) {
      sucreselectionne = "Peu (5g)"
    }
    else if (choixsucre == 3) {
      sucreselectionne = "Moyen (10g)"
    }
    else {
      sucreselectionne = "Beaucoup (15g)"
    }

    // personalisation de la boisson : choix du lait (que pour le cappuccino et latte)
    if ((choixboisson == 2) || (choixboisson == 3.1) || (choixboisson == 3.2) || (choixboisson == 3.3)) {
      println("Souhaitez vous ajouter du lait en supplément ?")
      println("1) oui")
      println("2) non")
      println(">")
      choixlait = readInt()
      prixtotal = prixboisson

      // vérfication que l'entrée est valide (pour le choix du lait)
      while (choixlait != 1 && choixlait != 2) {
        println("Veuillez sélectionner une entrée valide")
        println(">")
        choixlait = readInt()
      }

      // Personalisation de la boisson: choix du nombre de dose de lait (seulement si l'option 1 (Oui) est selectionée)
      // Changement de variables laitselectionne et prixlait pour faciliter l'affichage du recapitulatif et le calcul du prix
      if (choixlait == 1) {
        laitselectionne = "Oui"
        println("Combien de dose souhaitez vous ? Vous pouvez ajouter jusqu'à 3 dose maximum. 50 ml de lait et - CHF 0.05 par dose")
        println(">")
        choixdoselait = readInt()

        // vérification que l'entrée est valide (pour le choix du nombre de dose de lait)
        while (choixdoselait != 1 && choixdoselait != 2 && choixdoselait != 3) {
          println("Veuillez sélectionner une entrée valide")
          println(">")
          choixdoselait = readInt()
        }

        // changement de la variable laiterequis pour faciliter le calcul des stock
        // changement de la variable prixlait pour faciliter l'affichage du recapitulatif et le calcul du prix
        laitrequis += choixdoselait * 50 // formule générale qui englobe les trois cas possibles (permet de simplifier et réduire les instructions)
        prixlait = choixdoselait * 0.050
      }

      if (choixlait == 2) {
        laitselectionne = "Non"
        prixlait = 0.0
        choixdoselait = 0
      }
    }

    // Vérification des stock: affichage du récapitulatif de la boisson sans les prix et messages d'erreurs
    if ((stockcafe(machineId) < caferequis) || (stocksucre(machineId) < sucrerequis) || (stocklait(machineId) < laitrequis)) {
      if (choixboisson == 1) {
        println("Boisson sélectionnée : " + boissonselectionnee1)
        println("Niveau de sucre : " + sucreselectionne)
        println()
      }
      if ((choixboisson == 2) || (choixboisson == 3.1) || (choixboisson == 3.2) || (choixboisson == 3.3)) {
        println("Boisson sélectionnée : " + boissonselectionnee1)
        println("Niveau de sucre : " + sucreselectionne)
        println("Lait supplémentaire : " + laitselectionne)
        println()
      }
      if (stockcafe(machineId) < caferequis) {
        if ((choixboisson == 1) || (choixboisson == 2) || (choixboisson == 3.1)) {
          println("Erreur : quantité de poudre de café insuiffisante pour préparer la boisson séléctionnée.")
          println("Veuillez choisir une autre machine ou verifier les stocks en mode Admin.")
          println()
        }
        else {
          println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson séléctionnée.")
          println("Veuillez choisir une taille plus petite, choisir une autre machine ou verifier les stocks en mode Admin.")
          println()
        }
        stocksuffisant = 2
        return false //changement de la variable stocksuffisant pour revenir au menu client si les stocks sont insuffisants
      }

      if (stocksucre(machineId) < sucrerequis) {
        println("Erreur : quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
        println("Veuillez choisir une autre machine ou verifier les stocks en mode Admin.")
        println()

        stocksuffisant = 2
        return false
      }

      if (stocklait(machineId) < laitrequis) {
        if ((choixboisson == 2) || (choixboisson == 3.1)) {
          println("Erreur : quantité de lait insuffisante pour préprarer la boisson séléctionnée.")
          println("Veuillez choisir une autre machine ou verifier les stocks en mode Admin.")
          println()
        }
        if ((choixboisson == 3.2) || (choixboisson == 3.3)) {
          println("Erreur : quantité de lait insuffisante pour préparer la boisson séléctionnée.")
          println("Veuillez choisir une taille plus petite, choisir une autre machine ou vérifier les stocks en mode Admin.")
          println()
        }
        stocksuffisant = 2
        return false
      }
    }

    // Affichage du recapitulatif avec les prix (une fois que les stocks sont validés)
    // Comparaison avec la variable stocksuffisant pour permetre de continuer seulement si les stocks suffisent
    if (stocksuffisant == 1) {
      prixtotal = prixboisson + prixsucre + prixlait

      if (choixboisson == 1) {
        println("Boisson sélectionnée : " + boissonselectionnee1)
        println("Niveau de sucre : " + sucreselectionne)
        if (choixsucre == 1) {
          printf("Prix total : CHF %.2f", prixtotal) // Affichage du prix à deux décimales pour assurer un prix aux centimes (cohérent)
        }
        else {
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixtotal)
        }
      }

      else {
        println("Boisson sélectionnée : " + boissonselectionnee1)
        println("Niveau de sucre : " + sucreselectionne)
        println("lait supplémentaire : " + laitselectionne)

        if (choixsucre == 1 && choixlait == 2)
          printf("Prix total: CHF %.2f", prixtotal)
        if ((choixsucre >= 2 && choixsucre <= 4) && (choixlait == 2))
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixtotal)
        if (choixsucre == 1 && choixlait == 1)
          printf("Prix total :  CHF %.2f + CHF %.2f  =  CHF %.2f", prixboisson, prixlait, prixtotal)
        if ((choixsucre >= 2 && choixsucre <= 4) && (choixlait == 1))
          printf("Prix total :  CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixlait, prixtotal)
      }

      // Paiement avec un code Twint aléatoire: utilisation d'une boucle for pour additionner 5 caractères
      codeTwint = "" // initialisation du code
      for (_ <- 1 to longeurcodeTwint) {
        var alphanumericaleatoire = alphanumeric(Random.nextInt(alphanumeric.length))
        codeTwint += alphanumericaleatoire
      }
      println()
      println()
      println("Veuillez payer en utilisant Twint.")
      println("Votre code de paiement est : " + codeTwint)
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000) // attente de 3 secondes (3000 millisecondes)
      println("          ")
      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson...")
      println("[...]")
      println("Votre " + boissonselectionnee + " est prêt ! Bonne dégustation !")
      println()

      // Déduction des stock de la machine après la transaction
      stockcafe(machineId) -= caferequis
      stocklait(machineId) -= laitrequis
      stocksucre(machineId) -= sucrerequis
    }
    return true
  }

  // 1.4) méthode pour remplir les stocks en mode Admin selon la machine séléctionnée
  def restockMachine(machineId: Int, stockcafe: Array[Int], stocksucre: Array[Int], stocklait: Array[Int]): Unit = {

    // Affichage des stocks actuels (seulement lorsque le code PIN est validé)
    println()
    println("Niveaux de stock actuels : ")
    println("Poudre de café : " + stockcafe(machineId) + "g")
    printf("Lait :  %.3fL", stocklait(machineId) / 1000.0) // affichage du lait à 3 décimales pour la cohérence des mesures et /1000 pour l'affichage en L
    println()
    println("Sucre : " + stocksucre(machineId) + "g")
    println()

    // Réaprovisionnement des stocks: verification que les entrées sont valides et changement des variables stockcafe, stocksucre et stocklait pour enregistrer les ajouts
    println("Veuillez entrer les quantités à ajouter : ")
    println("Poudre de café : ")
    println(">")
    var ajoutcafe = readInt()
    while (ajoutcafe < 0) {
      println("Veuillez entrer une valeur positive")
      println(">")
      ajoutcafe = readInt()
    }
    stockcafe(machineId) += ajoutcafe

    println("Lait : ")
    println(">")
    var ajoutlait = (readDouble() * 1000).toInt // * 1000 pour stocker le lait en ml
    while (ajoutlait < 0) {
      println("Veuillez entrer une valeur positive")
      println(">")
      ajoutlait = (readDouble() * 1000).toInt
    }
    stocklait(machineId) += ajoutlait

    println("Sucre : ")
    println(">")
    var ajoutsucre = readInt()
    while (ajoutsucre < 0) {
      println("Veuillez entrer une valeur positive")
      println(">")
      ajoutsucre = readInt()
    }
    stocksucre(machineId) += ajoutsucre

    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
    println()
  }

  def main(args: Array[String]): Unit = {
    // Etape 2: rédaction du code et appel des méthodes

    // variables pour le choix du mode et de la machine
    var choixmode = 0
    var machineId = 0

    // variables pour la gestion des stocks selon les différentes machines
    val nbmachines = 5
    val stockcafe = Array.fill(nbmachines)(50)
    val stocksucre = Array.fill(nbmachines)(30)
    val stocklait = Array.fill(nbmachines)(500)
    val machinePins = Array.fill(nbmachines)("434343")

    // variable pour le mode Admin
    var choixaction = 0

    // 2.1) sélection du mode et de la machine
    // Sélection du mode: arrêt du programme lors du choix "quitter" (3)
    while (choixmode != 3) {
      println()
      println("       Nospresso Café    ")
      println("Veuillez sélectionner votre mode")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")
      choixmode = readInt()

      // vérification que l'entrée est valide (pour le choix du mode)
      while (choixmode != 1 && choixmode != 2 && choixmode != 3) {
        println("Veuillez sélectionner une entrée valide")
        println(">")
        choixmode = readInt()
      }
      // séléction de la machine (pour le mode Admin et le mode client)
      if (choixmode == 1 || choixmode == 2) {
        println("Veuillez séléctionner une machine (1 - 5)")
        println(">")
        machineId = readInt() - 1

        // verification que l'entrée est valide (pour le choix de la machine)
        while (machineId != 0 && machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4) {
          println("Veuillez sélectionner une entrée valide")
          println(">")
          machineId = readInt() - 1
        }

        // 2.2) mode Client
        // appel de la méthode 3 et retour au choix de la machine si le stock est insuffisant
        if (choixmode == 1) {
          var stockvalide = serveClient(machineId, stockcafe, stocksucre, stocklait)
          while (!stockvalide) {
            println("Veuillez sélectionner une machine (1 - 5)")
            println(">")
            machineId = readInt() - 1

            // verification que l'entrée est valide (pour le choix de la machine)
            while (machineId != 0 && machineId != 1 && machineId != 2 && machineId != 3 && machineId != 4) {
              println("Veuillez sélectionner une entrée valide")
              println(">")
              machineId = readInt() - 1
            }
            stockvalide = serveClient(machineId, stockcafe, stocksucre, stocklait)
          }
        }
        // 2.3) mode Admin
        // appel de la méthode 1,2 et 4 et arrêt du programme lorsque le code PIN est faux (après les 3 tentatives)
        if (choixmode == 2) {
          if (validatePin(machineId, machinePins)) {
            println()
            println("Que souhaitez-vous faire ? ")
            println("1) Réaprovisionner les stocks ")
            println("2) mettre à jour le code PIN ")
            println(">")
            choixaction = readInt()

            while (choixaction != 1 && choixaction != 2) {
              println("Veuillez sélectionner une entrée valide")
              println(">")
              choixaction = readInt()
            }

            if (choixaction == 2) {
              updatePin(machineId, machinePins)
            }
            if (choixaction == 1) {
              restockMachine(machineId, stockcafe, stocksucre, stocklait)
            }
          } else {
            choixmode = 3 // arret du programme si les tentatives sont échouéées
          }
        }
      }
    }
  }
}

