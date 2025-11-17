import java.io.PrintWriter
import scala.io.Source
import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer

class Machine(val id: Int, var pincode: String, var milk:Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    ingredient.toLowerCase match {
      case "coffee" => coffee += amount
      case "milk" => milk += amount
      case "sugar" => sugar += amount
      case _ => false
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    ingredient.toLowerCase match {
      case "coffe" if coffee >= amount => coffee -= amount; true
      case "milk" if milk >= amount => milk -= amount; true
      case "sugar" if sugar >= amount => sugar -= amount; true
      case _ =>
        println("Stock insuffisant, veuillez choisir une autre boisson "); false
    }
  }
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val lines = Source.fromFile(filename).getLines().toList
      if (lines.isEmpty) throw new Exception("Le fichier CSV est vide.")


      for ((line, i) <- lines.zipWithIndex if i > 0) {
        val colonnes = line.split(",").map(_.trim)
        val machine = new Machine(i, colonnes(0), colonnes(1), colonnes(3).toInt)
        machines += machine
      }

        object Nospressocafe {
          def main(args: Array[String]): Unit = {
val filename = "machine.csv"
            val machine = loadcsv(filename)
            val client = 1
            val admin = 2
            val quitter = 3
            var mode = 0

            val nbmachines = 5

            //  Stock, identifiants et codes pin

            var machineId = 0
            var coffeestocks = Array.fill(nbmachines)(50)
            var sugarstocks = Array.fill(nbmachines)(30)
            var milkstocks = Array.fill(nbmachines)(500)
            val machinepins = Array.fill(nbmachines)(434343)
            var choixmachine = 0

            // Consommation des ingrédients
            val consommationexpresso = 8
            val consommationcapuccino = 6
            val consocaffeptitlatte = 6
            val consocaffemoylatte = 8
            val consocaffegrandlatte = 12
            val consolaitcappucccino = 100
            val consolaitpetitlatte = 120
            val consolaitmoylatte = 150
            val consolaitgrandlatte = 200
            val consolaitsup =50

            val consosucrepeu = 5
            val consosucremoy = 10
            val consosucrebcp= 15

            var demarage = true
            while(demarage) {
              do {
                println("Nospresso Cafe")
                println("Veuillez sélectionner votre mode:")
                println("1) Client")
                println("2) Admin")
                println("3) Quitter")
                mode = readLine(">").toInt

              } while (mode !=client && mode != admin && mode != quitter)

              if (mode == client) {

                do {
                  println("Quel machine choisissez-vous?")

                  choixmachine = readLine("<").toInt
                  if (choixmachine < 0 || choixmachine > nbmachines-1) {

                    println("Veuillez choisir une machine existante")
                  }
                }while(choixmachine < 0 || choixmachine > nbmachines-1)

                machineId = choixmachine

                def serveClient(machineId: Int, coffeestocks: Array[Int], sugarstocks: Array[Int], milkstocks: Array[Int]): Boolean = {


                  if (serveClient(machineId, coffeestocks, sugarstocks, milkstocks)) {
                    var boissoncommande = ""
                    var expresso = 1
                    var cappuccino = 2
                    var latte = 3
                    var choixboisson = 0

                    do {
                      println("Veuillez sélectionner votre boisson:")
                      println("1) Expresso - CHF 2.00")
                      println("2) Cappuccino - CHF 2.50")
                      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
                      choixboisson = readLine(">").toInt
                    } while (choixboisson != expresso && choixboisson != cappuccino && choixboisson != latte)

                    if (choixboisson == expresso) {

                      coffeestocks(machineId) -= consommationexpresso
                      boissoncommande = "Expresso"


                      if (coffeestocks(machineId) < consommationexpresso) {
                        do {
                          println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionné.")
                          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          return false
                        } while (coffeestocks(machineId) > consommationexpresso)
                      }
                    }

                    if (choixboisson == cappuccino) {
                      coffeestocks(machineId) -= consommationcapuccino
                      milkstocks(machineId) -= consolaitcappucccino
                      boissoncommande = "Capuccino"


                      if (coffeestocks(machineId) < consommationcapuccino || milkstocks(machineId) < 0.10) {
                        do {
                          println("Erreur: Quantité de poudre à café ou quantité de lait insuffisante")


                          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          return false
                        } while (coffeestocks(machineId) > consommationcapuccino)
                      }
                    }
                    var choixlatte = 0
                    var petitlatte = 1
                    var moyenlatte = 2
                    var grandlatte = 3


                    if (choixboisson == latte) {
                      boissoncommande = "Latte"
                      do {
                        println("Quel taille de Latte ?")
                        println("1) Petit")
                        println("2) Moyen")
                        println("3) Grand")
                        choixlatte = readLine(">").toInt
                      } while (choixlatte != petitlatte && choixlatte != moyenlatte && choixlatte != grandlatte)
                      if (choixlatte == petitlatte) {

                        coffeestocks(machineId) -= consocaffeptitlatte
                        milkstocks(machineId) -= consolaitpetitlatte
                        boissoncommande = "Petit Latte"

                      }
                      if (choixlatte == moyenlatte) {
                        coffeestocks(machineId) -= consocaffemoylatte
                        milkstocks(machineId) -= consolaitmoylatte
                        boissoncommande = "Moyen Latte"
                      }
                      if (choixlatte == grandlatte) {
                        coffeestocks(machineId) -= consocaffegrandlatte
                        milkstocks(machineId) -= consolaitgrandlatte
                        boissoncommande = "Latte Grand"
                      }
                      if (choixlatte == petitlatte && coffeestocks(machineId) < consocaffeptitlatte && milkstocks(machineId) < consolaitpetitlatte || choixlatte == moyenlatte && coffeestocks(machineId) < consocaffemoylatte && milkstocks(machineId< consolaitmoylatte || choixlatte == grandlatte && coffeestocks(machineId) < consocaffegrandlatte && milkstocks(machineId) < consolaitgrandlatte) {
                        println("Erreur: Quantité de poudre à café ou quantité de lait insuffisante")


                        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                        return false

                      }
                    }
                    var sanssucre = 1
                    var peu = 2
                    var moyen = 3
                    var beaucoup = 4
                    var choixsucre = 0
                    do {
                      println("Souhaitez-vous ajouter du sucre ?")
                      println("1) Sans sucre")
                      println("2) Peu (5g) - CHF 0.10")
                      println("3) Moyen (10g) - CHF 0.20")
                      println("4) Beaucoup (15g) - CHF 0.30")
                      choixsucre = readLine(">").toInt
                    } while (choixsucre != sanssucre && choixsucre != peu && choixsucre != moyen && choixsucre != beaucoup)
                    var dosesucre = ""
                    if (choixsucre == 2) {
                      sugarstocks(machineId) -= consosucrepeu
                      dosesucre = "Peu(5g)"

                    }
                    if (choixsucre == 3) {
                      sugarstocks(machineId) -= consosucremoy
                      dosesucre = "Moyen(10g)"
                    }
                    if (choixsucre == 4) {
                      sugarstocks(machineId) -= consosucrebcp
                      dosesucre = "Beaucoup(15g)"
                    }
                    if (choixsucre == peu && sugarstocks(machineId) < consosucrepeu || choixsucre == moyen && sugarstocks(machineId) < consosucremoy || choixsucre == beaucoup && sugarstocks(machineId) < consosucrebcp) {
                      do {
                        println("Erreur: Quantité de sucre insuffisante")
                        println("Veuillez choisir une autre quantité de sucre ou vérifier les stocks en mode Admin.")
                        System.exit(0)
                      } while (choixsucre == peu && sugarstocks(machineId) >= consosucrepeu || choixsucre == moyen && sugarstocks(machineId) >= consosucremoy || choixsucre == beaucoup && sugarstocks(machineId) >= consosucrebcp)

                    }
                    if (choixboisson == cappuccino || choixboisson == latte) {
                      var oui = 1
                      var non = 2
                      var laitsup = 0
                      do {
                        println("Souhaitez-vous ajouter du lait supplémentaire ?")
                        println("1) Oui")
                        println("2) Non")
                        laitsup = readLine(">").toInt
                      } while (laitsup != oui && laitsup != non)

                      if (laitsup == oui) {
                        var dose = 0
                        println("Combien de dose ?")
                        dose = readLine(">").toInt
                        milkstocks(machineId) -= dose * consolaitsup
                        if (milkstocks(machineId) < (dose * consolaitsup)) {
                          do {
                            println("Quantité de lait insuffisante")
                            return false
                          } while (milkstocks(machineId) >= (dose * consolaitsup))
                        }
                      }
                    }


                    println("Boisson commandé:" + boissoncommande)
                    println("Niveau de sucre:" + dosesucre)


                    println("Veuillez payer en utilisant Twint.")
                    val alphanumerique = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
                    var codepaiement = ""
                    for (i <- 1 to 5) {
                      val stringaleatoire = alphanumerique((math.random * alphanumerique.length).toInt).toString
                      codepaiement.+=(stringaleatoire)
                    }
                    printf("Votre code de paiement est: %s\n", codepaiement)
                    println("En attente de validation du paiement...")
                    Thread.sleep(5000)
                    println("Merci ! Votre paiement a été accepté.")

                    println("Péparation de votre boisson...")
                    Thread.sleep(5000)
                    println("Votre " + boissoncommande + "est prêt ! Bonne dégustation !")
                  }

                }
              }
              if (mode == quitter){
                System.exit(0)
              }
              if (mode == admin) {
                if (mode == admin) {
                  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {

                  }
                  println(" entrer le code PIN de la machine :")
                  if (validatePin(machineId, )) {
                    println("Accès admin autorisé.")
                    println("1) Recharger les stocks")
                    println("2) Modifier le code PIN")
                  }
          }
        }
