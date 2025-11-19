import scala.util.Random
import io.StdIn._
object Nospresso {
  val nbmachine = 5
  val pin = Array("434343", "434343", "434343", "434343", "434343")
  val cstock = Array(50, 50, 50, 50, 50)
  val sstock = Array(30, 30, 30, 30, 30)
  val lstock = Array(500, 500, 500, 500, 500)
  def main(args: Array[String]): Unit = {
    var encore = true
    while (encore) {
      println("Veuillez choisir votre mode ")
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")
      println(">")
      val choix = readLine()
      if (choix == "1") {
        println("Machine sélectionnée (1-" + nbmachine + ") >")
        val machineid = readInt() - 1
        if (machineid >= 0 && machineid < nbmachine) {
          println("Accès à la Machine " + (machineid + 1) + " accordé.")
          val result = servClient(machineid, cstock, sstock, lstock)
          if (!result) println("Transaction échouée.")
        } else {
          println("Numéro de machine invalide. Choisissez entre 1 et " + nbmachine + ".")
        }
      } else if (choix == "2") {
        println("Machine sélectionnée (1-" + nbmachine + ") >")
        val machineid = readInt() - 1
        if (machineid >= 0 && machineid < nbmachine) {
          println("Accès à la Machine " + (machineid + 1) + " accordé.")
          if (validepin(machineid)) {
            println("1) Réapprovisionnement des stocks")
            println("2) Changer le code PIN")
            val choixadmin = readInt()
            if (choixadmin == 1) {
              restockmachine(machineid, cstock, sstock, lstock)
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
        println("Danke, Hasta la vista")
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
      if (saisidupin == pin(machineId)) {
        println("Accès accordé à la Machine " + (machineId + 1) + ".")
        return true
      } else {
        tent -= 1
        if (tent > 0) {
          println("Code incorrect. Tentatives restantes : " + tent)
        } else {
          println("Code PIN incorrect. 0 tentatives restantes.")
          println("Trop de tentatives. Fin du programe.")
          return false
        }
      }
    }
    false
  }
  def updatepin(machineID: Int): Unit = {
    println("Entrez un nouveauPIN à 6 chiffres :")
    var nouveaupin = ""
    do {
      nouveaupin = readLine()
      if (nouveaupin.length == 6 && nouveaupin.forall(_.isDigit)) {
        pin(machineID) = nouveaupin
        println("Code PIN mis à jour avec succès.")
      } else {
        println("Le code doit comporter 6 chiffres.")
      }
    } while (nouveaupin.length != 6 || !nouveaupin.forall(_.isDigit))
  }
  def servClient(machineid: Int, caffeestocks: Array[Int], sucreStocks: Array[Int], laitstock: Array[Int]): Boolean = {
    println("Choisissez votre boisson : ")
    println("1) Expresso (2.00 CHF)")
    println("2) Cappuccino (2.50 CHF)")
    println("3) Latte (à partir de 3.00 CHF)")
    val boisson = readInt()
    var cafededans = 0
    var laitdedans = 0
    var prix = 0.0
    var nomboi = ""
    if (boisson == 1) {
      nomboi = "Expresso"
      cafededans = 8
      prix = 2.0
    } else if (boisson == 2) {
      nomboi = "Cappuccino"
      cafededans = 6
      laitdedans = 100
      prix = 2.5
    } else if (boisson == 3) {
      nomboi = "Latte"
      println("Choisissez la taille : (1) Petit (3.00 CHF), (2) Moyen (3.50 CHF), (3) Grand (4.00 CHF)")
      val taille = readInt()
      if (taille == 1) {
        cafededans = 6; laitdedans = 120; prix = 3.0
      } else if (taille == 2) {
        cafededans = 8; laitdedans = 150; prix = 3.5
      } else if (taille == 3) {
        cafededans = 12; laitdedans = 200; prix = 4.0
      } else {
        println("Taille invalide.")
        return false
      }
    } else {
      println("Choix de boisson invalide.")
      return false
    }
    var laitsupp = 0
    var plaitsupp = 0.0
    if (boisson == 2 || boisson == 3) {
      println("Souhaitez-vous du lait supplémentaire ? (1) Oui / (2) Non :")
      val replait = readInt()
      if (replait == 1) {
        println("Combien de doses supplémentaires de lait ? (1-3 doses, 50ml par dose) :")
        val dose = readInt()
        if (dose >= 1 && dose <= 3) {
          laitsupp = dose * 50
          plaitsupp = dose * 0.05
        } else {
          println("Nombre de doses invalide. Aucune dose supplémentaire ajoutée.")
        }
      }
    }
    val sucresupp = Array(0, 0, 5, 10, 15)
    val sucrep = Array(0.0, 0.0, 0.1, 0.2, 0.3)
    println("Quantité de sucre : (1) Pas de sucre, (2) Peu, (3) Moyen, (4) Beaucoup")
    val sucredose = readInt()
    if (sucredose >= 1 && sucredose <= 4) {
      val sucreUtilise = sucresupp(sucredose)
      val pSucre = sucrep(sucredose)

      if (caffeestocks(machineid) >= cafededans &&
        laitstock(machineid) >= (laitdedans + laitsupp) &&
        sucreStocks(machineid) >= sucreUtilise) {
        val codeT = Random.alphanumeric.take(5).mkString.toUpperCase
        println("Veuillez payer en utilisant Twint.")
        println("Votre code de paiement est : " + codeT)
        println("En attente de validation du paiement...")
        Thread.sleep(3000)
        caffeestocks(machineid) -= cafededans
        laitstock(machineid) -= (laitdedans + laitsupp)
        sucreStocks(machineid) -= sucreUtilise
        val prixTotal = prix + pSucre + plaitsupp
        println(f"Prix total : $prixTotal%.2f CHF")
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
    println("Stock actuels de la machine " + (machineid + 1) + ":")
    println("Café : " + caffeestocks(machineid) + "g")
    println("Sucre : " + sucreStocks(machineid) + "g")
    println("Lait : " + laitstock(machineid) + "ml")
    println("Quantité de café à ajouter (g) : ")
    val cajout = readInt()
    caffeestocks(machineid) += cajout
    println("Quantité de sucre à ajouter (g) : ")
    val sajout = readInt()
    sucreStocks(machineid) += sajout
    println("Quantité de lait à ajouter (ml) : ")
    val lajout = readInt()
    laitstock(machineid) += lajout
    println("Stocks mis à jour avec succès.")
  }
}