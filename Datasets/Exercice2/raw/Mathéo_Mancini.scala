import scala.util.Random
import io.StdIn._

object Nospresso {
  val nbmemachines = 5
  val machinePins = Array("434343", "434343", "434343", "434343", "434343")
  val caffeestocks = Array(50, 50, 50, 50, 50)
  val sucreStocks = Array(30, 30, 30, 30, 30)
  val laitstock = Array(500, 500, 500, 500, 500)

  def main(args: Array[String]): Unit = {

    var encore = true
    while (encore) {
      println("Veuillez choisir votre mode ")
      println("1) mode client")
      println("2)mode admin")
      println("3) Quitter")
      println(">")
      val choix = readLine()

      if (choix == "1") {
        println("Machine sélectionnée (1-" + nbmemachines +")")
        println(">")
        val machineid = readInt()
        if (machineid >= 0 && machineid < nbmemachines) {
          println("Accès à la Machine " + machineid + " accordé.")
          val result = servClient(machineid, caffeestocks, sucreStocks, laitstock)
          if (!result) println("Transaction échouée.")
        } else {
          println("Numéro de machine invalide."+ (nbmemachines-1))
        }
      } else if (choix == "2") {
        println("Machine sélectionnée (1-" + nbmemachines +")")
        println(">")
        val machineid = readInt()
        if (machineid >= 0 && machineid < nbmemachines) {
          println("Accès à la Machine " + machineid + " accordé.")
          if (validepin(machineid)) {
            println("1) Réapprovisionnement des stocks")
            println("2) Changer le code PIN")
            val choixadmin = readInt()
            if (choixadmin == 1) {
              restockmachine(machineid, caffeestocks, sucreStocks, laitstock)
            } else if (choixadmin == 2) {
              updatepin(machineid)
            } else {
              println("Option invalide.")
            }
          } else {
            println("Accès refusé. Code PIN incorrect.")
          }
        } else {
          println("Numéro de machine invalide.")
        }
      } else if (choix == "3") {
        println("Merci et au revoir.")
        encore = false
      } else {
        println("Option invalide.")
      }
    }
  }

  def validepin(machineId: Int): Boolean = {
    var tent = 3
    while (tent > 0) {
      println("Entrez le code PIN : ")
      val saisidupin = readLine()
      if (saisidupin == machinePins(machineId)){
        println("Accès accordé à la Machine " + machineId + ".")


        return true
      } else {
        tent -= 1
        if (tent > 0) {
          println("Code incorrect. Tentatives restantes : " + tent)
        } else {
          println("Code PIN incorrect. 0 tentatives restantes.")
          println("Trop de tentatives échouées. Fin du programme.")
          return false
        }
      }
    }
    false
  }

  def updatepin(machineID: Int): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    var nouveaupin = ""
    do {
      nouveaupin = readLine()
      if (nouveaupin.length == 6 && nouveaupin.forall(_.isDigit)) {
        machinePins(machineID) = nouveaupin
        println("Code PIN mis à jour avec succès.")
      } else {
        println("Le code doit comporter exactement 6 chiffres.")
      }
    } while (nouveaupin.length != 6 || !nouveaupin.forall(_.isDigit))
  }

  def servClient(machineid: Int, caffeestocks: Array[Int], sucreStocks: Array[Int], laitstock: Array[Int]): Boolean = {
    println("Choisissez votre boisson : ")
    println("1) Expresso - (2.00 CHF)")
    println("2)Cappucinno- ( 2.5)CHF")
    println("3) Latte - (3.00 CHF)")

    val boichoisi = readInt()
    var cafeNecessaire = 0
    var laitNecessaire = 0
    var prixBase = 0.0
    var nomboi = ""

    if (boichoisi == 1) {
      nomboi = "Expresso"
      cafeNecessaire = 8
      prixBase = 2.0
    } else if (boichoisi == 2) {
      nomboi = "Cappuccino"
      cafeNecessaire = 6
      laitNecessaire = 100
      prixBase = 2.5
    } else if (boichoisi == 3) {
      nomboi = "Latte"
      println("Choisissez la taille : (1) Petit (3.00 CHF), (2) Moyen (3.50 CHF), (3) Grand (4.00 CHF)")
      val taille = readInt()
      if (taille == 1) {
        cafeNecessaire = 6; laitNecessaire = 120; prixBase = 2.7
      } else if (taille == 2) {
        cafeNecessaire = 8; laitNecessaire = 150; prixBase = 3.2
      } else if (taille == 3) {
        cafeNecessaire = 12; laitNecessaire = 200; prixBase = 3.7
      } else {
        println("Taille invalide.")
        return false
      }
    } else {
      println("Choix de boisson invalide.")
      return false
    }


    var laitsupplement = 0
    var prixLaitSupplement = 0.0
    // Lait supplémentaire
    if (boichoisi == 2 || boichoisi == 3) {
      println("Souhaitez-vous du lait supplémentaire ? ")
      println("1)oui")
      println("2) non")
      val reponseLait = readInt()
      if (reponseLait == 1) {
        println("Combien de doses supplémentaires de lait ? (1) Une dose, (2) Deux doses, (3) Trois doses : ")
        val doses = readInt()
        if (doses >= 1 && doses <= 3) {
          laitsupplement = doses * 50
          prixLaitSupplement = doses * 0.05
        } else {
          println("Nombre de doses invalide. Aucune dose supplémentaire ajoutée.")
        }
      }
    }




    val sucrequantites = Array(0, 0, 5, 10, 15)
    val sucreprix = Array(0.0, 0.0, 0.1, 0.2, 0.3)
    println(" 1)pas de sucre ")
    println("2)peu de sucre 5g")
    println(" 3)moyen 10g")
    println("4)beaucoup de sucre 15g")
    val sucrechoix = readInt()


    if (sucrechoix >= 1 && sucrechoix <= 4) {
      val sucreUtilise = sucrequantites(sucrechoix)
      val prixSucre = sucreprix(sucrechoix)


      if (caffeestocks(machineid) >= cafeNecessaire &&
        laitstock(machineid) >= (laitNecessaire + laitsupplement) &&
        sucreStocks(machineid) >= sucreUtilise) {

        val codeTwint = Random.alphanumeric.take(5).mkString.toUpperCase
        println("Veuillez payer en utilisant Twint.")
        println("Votre code de paiement est : " + codeTwint)
        println("En attente de validation du paiement...")
        Thread.sleep(3000)

        caffeestocks(machineid) = caffeestocks(machineid) - cafeNecessaire
        laitstock(machineid) = laitstock(machineid) - laitNecessaire
        sucreStocks(machineid) = sucreStocks(machineid) - sucreUtilise

        val prixTotal = prixBase + prixSucre + prixLaitSupplement
        println("Prix total : " + prixTotal + " CHF")

        println("Votre boisson " + nomboi + " est prête.")
        return true
      } else {
        println("Stock insuffisant pour préparer la boisson.")
        return false
      }
    } else {
      println("Quantité de sucre invalide.")
      return false
    }
  }

  def restockmachine(machineid: Int, caffeestocks: Array[Int], sucreStocks: Array[Int], laitstock: Array[Int]): Unit = {
    println("Stock actuels"+ machineid)
    println("Poudre a cafe: " + caffeestocks(machineid) + "g")
    println("Sucre : " + sucreStocks(machineid) + "g")
    println("Quantité de café à ajouter (g) : ")
    println("Lait" + laitstock(machineid) + "ml")

    println("Voulez-vous rajouter du sucre ? (1) Oui / (2) Non : ")
    val reponseSucre = readLine()
    if (reponseSucre == "1") {
      println("Quantité de sucre à ajouter (g) : ")
      val sucreAjout = readInt()
      sucreStocks(machineid) = sucreStocks(machineid) + sucreAjout
      println("Sucre ajouté avec succès.")
    }


    println("Voulez-vous rajouter du lait ? (1) Oui / (2) Non : ")
    val reponseLait = readLine()
    if (reponseLait == "1") {
      println("Quantité de lait à ajouter (ml) : ")
      val laitAjout = readInt()
      laitstock(machineid) = laitstock(machineid) + laitAjout
      println("Lait ajouté avec succès.")
    }
    println("Voulez-vous rajouter du café ? (1) Oui / (2) Non : ")
    val reponseCafe = readLine()
    if (reponseCafe == "1") {
      println("Quantité de café à ajouter (g) : ")
      val cafeAjout = readInt()
      caffeestocks(machineid) = caffeestocks(machineid) + cafeAjout
      println("Café ajouté avec succès. Nouveau stock : " + caffeestocks(machineid) + " g")
    }


    println("Stocks mis à jour avec succès.")
  }
}