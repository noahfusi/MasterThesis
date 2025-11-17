import scala.io.StdIn._
import scala.util.Random
import java.io.{FileNotFoundException, IOException, PrintWriter}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object main {
  class Machine(val id: Int, var pincode: String, var milk: Int,
                var sugar: Int, var coffee: Int) {

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "milk") milk += amount
      else if (ingredient == "sugar") sugar += amount
      else if (ingredient == "coffee") coffee += amount
      else println("Ingrédient invalide.")
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if(amount < 0) return false
      else if(ingredient == "milk" && milk < amount) return false
      else if(ingredient == "sugar" && sugar < amount) return false
      else if(ingredient == "coffee" && coffee < amount) return false
      else if(ingredient == "milk") {
        milk -= amount
        return true
      }
      else if(ingredient == "sugar") {
        sugar -= amount
        return true
      }
      else if(ingredient == "coffee") {
        coffee -= amount
        return true
      }
      else return false
    }

    def validatePin(): Boolean = {
      var essais = 3
      while(essais > 0){
        val pin = readLine("Entrez le code PIN : \n> ")
        if(pin == pincode){
          println("Accès accordé.")
          return true
        } else {
          essais -= 1
          println(f"Code PIN incorrect. ${essais} tentative(s) restante(s).")
        }
      }
      println("Trop de tentatives échouées. Fin du programme.")
      return false
    }

    def updatePin(): Unit = {
      println(f"Mise à jour du code PIN pour la Machine ${id}")
      var pin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      while(pin.length != 6 || !pin.forall(_.isDigit)){
        pin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      }
      pincode = pin
      println("Le code PIN a été mis à jour avec succès. \nRetour au menu principal...")
    }

    def restockMachine(): Unit = {
      println(f"\nStocks : \n   Café: ${coffee}}g \n   Lait: ${milk}mL \n   Sucre: ${sugar}g")

      println("\nRéapprovisionnement des stocks...\nAjout:")
      var ajoutCafe = readLine("   Poudre de café > ").toInt
      while(ajoutCafe < 0){
        println("La valeur doit être positive. Veuillez réessayer.")
        ajoutCafe = readLine("   Poudre de café > ").toInt
      }
      var ajoutLait = readLine("   Lait > ").toInt
      while(ajoutLait < 0){
        println("La valeur doit être positive. Veuillez réessayer.")
        ajoutLait = readLine("   Lait > ").toInt
      }

      var ajoutSucre = readLine("   Sucre > ").toInt
      while(ajoutSucre < 0){
        println("La valeur doit être positive. Veuillez réessayer.")
        ajoutSucre = readLine("   Sucre > ").toInt
      }

      addIngredient("coffee", ajoutCafe)
      addIngredient("milk", ajoutLait)
      addIngredient("sugar", ajoutSucre)

      println("Les stocks ont été mis à jour avec succès. \nRetour au menu principal...")

    }

    def serveClient(): Boolean = {
      var choixBoisson = readLine("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
      while(choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3){
        println("Choix invalide. Veuillez réessayer.")
        choixBoisson = readLine("Veuillez sélectionner votre boisson : \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
      }

      var cafeUtilise = 0
      var laitUtilise = 0
      var sucreUtilise = 0

      var prixBase = 0.0
      var prixSucre = 0.0
      var prixLait = 0.0

      var nomBoisson = "<Inconnu>"
      var niveauSucre = "Sans sucre"
      var supplementLait = "Non"

      // Choix de la boisson
      if (choixBoisson == 1) {
        cafeUtilise = 8
        prixBase = 2.0
        nomBoisson = "Expresso"
      } else if (choixBoisson == 2) {
        cafeUtilise = 6
        laitUtilise = 100
        prixBase = 2.5
        nomBoisson = "Cappuccino"
      } else if (choixBoisson == 3) {
        var tailleLatte = readLine("Choisissez la taille du Latte : \n1) Petit \n2) Moyen \n3) Grand \n> ").toInt
        while(tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3){
          println("Choix invalide. Veuillez réessayer.")
          tailleLatte = readLine("Choisissez la taille du Latte : \n1) Petit \n2) Moyen \n3) Grand \n> ").toInt
        }
        if (tailleLatte == 1) {
          cafeUtilise = 6
          laitUtilise = 120
          prixBase = 2.7
          nomBoisson = "Latte (Petit)"
        } else if (tailleLatte == 2) {
          cafeUtilise = 8
          laitUtilise = 150
          prixBase = 3.2
          nomBoisson = "Latte (Moyen)"
        } else if (tailleLatte == 3) {
          cafeUtilise = 12
          laitUtilise = 200
          prixBase = 3.7
          nomBoisson = "Latte (Grand)"
        }
      }

      // Choix du sucre
      var choixSucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
      while(choixSucre != 1 && choixSucre != 2 && choixSucre != 3 && choixSucre != 4){
        println("Choix invalide. Veuillez réessayer.")
        choixSucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
      }
      if (choixSucre == 2) {
        sucreUtilise = 5
        prixSucre = 0.1
        niveauSucre = "Peu (5g)"
      } else if (choixSucre == 3) {
        sucreUtilise = 10
        prixSucre = 0.2
        niveauSucre = "Moyen (10g)"
      } else if (choixSucre == 4) {
        sucreUtilise = 15
        prixSucre = 0.3
        niveauSucre = "Beaucoup (15g)"
      }

      // Choix du lait
      if(choixBoisson == 2 || choixBoisson == 3){
        var supplementLaitChoix = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n> ").toInt
        while(supplementLaitChoix != 1 && supplementLaitChoix != 2){
          println("Choix invalide. Veuillez réessayer.")
          supplementLaitChoix = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n> ").toInt
        }
        if (supplementLaitChoix == 1) {
          var supplementLaitDose = readLine("\nCombien de dose ? \n> ").toInt
          while(supplementLaitDose < 1 || supplementLaitDose > 3){
            println("Choix invalide. Maximum 3 doses. Veuillez réessayer.")
            supplementLaitDose = readLine("\nCombien de dose ? \n> ").toInt
          }
          laitUtilise += supplementLaitDose * 50
          prixLait = supplementLaitDose * 0.05
          supplementLait = f"$supplementLaitDose dose(s)"
        }
      }

      // Vériﬁcation des stocks
      if(coffee < cafeUtilise) println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")

      if(milk < laitUtilise) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")

      if(milk < sucreUtilise) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")

      if(coffee >= cafeUtilise && milk >= laitUtilise && sugar >= sucreUtilise){

        println(f"Boisson sélectionée : $nomBoisson \nNiveau de sucree : $niveauSucre \nLait supplémentaire : $supplementLait\n")
        println(f"Prix total : CHF $prixBase%.2f + CHF $prixSucre%.2f + CHF $prixLait%.2f = CHF ${prixBase + prixSucre + prixLait}%.2f")

        println("\nVeuillez payer en utilisant Twint.")
        println(f"Votre code de paiement est : ${Random.alphanumeric.take(5).mkString}")
        println("(En attente de paiement...)")

        Thread.sleep(3000)

        println("\nPaiement confirmé. \nPréparation de votre boisson...")

        removeIngredient("coffee", cafeUtilise)
        removeIngredient("milk", laitUtilise)
        removeIngredient("sugar", sucreUtilise)

        Thread.sleep(3000)

        println(f"Votre $nomBoisson est prêt ! Bonne dégustation.")
        return true

      }
      else{
        println("Les stocks pour préparer votre boissons sont insuffisants. Veuillez éessayer une autre boisson ou une autre machine.")
        return false
      }
    }
  }



  def choixMachine(nbMachines : Int): Int = {
    var choixMachine = readLine(f"Machine sélectionée (1-$nbMachines) > ").toInt
    while(choixMachine < 1 || choixMachine > nbMachines){
      println("Choix invalide. Veuillez réessayer.")
      choixMachine = readLine(f"Machine sélectionée (1-$nbMachines) > ").toInt
    }
    return choixMachine - 1 // Index des tableaux commence à 0
  }

  def choixMenu(): Int = {
    var choixMenu = readLine("\n      Nospresso Café \nVeuillez sélectionner votre mode :\n1) Client \n2) Admin \n3) Quitter \n> ").toInt
    while(choixMenu != 1 && choixMenu != 2 && choixMenu != 3){
      println("Choix invalide. Veuillez réessayer.")
      choixMenu = readLine("\n      Nospresso Café \nVeuillez sélectionner votre mode :\n1) Client \n2) Admin \n3) Quitter \n> ").toInt
    }
    return choixMenu
  }

  def choixAdmin(): Int = {
    var choixAdmin = readLine("\nMode Admin\n1) Mettre à jour le code PIN\n2) Réapprovisionner les stocks\n> ").toInt
    while(choixAdmin != 1 && choixAdmin != 2){
      println("Choix invalide. Veuillez réessayer.")
      choixAdmin = readLine("\nMode Admin\n1) Mettre à jour le code PIN\n2) Réapprovisionner les stocks\n> ").toInt
    }
    return choixAdmin
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val fw = new PrintWriter(filename)
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for(m <- machines){
        fw.println(f"${m.pincode},${m.milk},${m.sugar},${m.coffee}")
      }
      println(s"Sauvegarde de ${machines.length} machines dans $filename...")
      println("Fichier sauvegardé avec succès.")
      fw.close()
    } catch {
      case _: FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
      case _: IOException =>
        println("Erreur : Échec de l'écriture dans le fichier. Le fichier peut être verrouillé ou en lecture seule.")
        System.exit(1)
      case e: Exception =>
        println(s"Erreur inattendue : ${e.getMessage}")
        System.exit(1)
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println(s"Chargement des machines depuis $filename...")
    var machines = ArrayBuffer[Machine]()
    var id = 1
    try {
      val lignes = Source.fromFile(filename).getLines().drop(1) // Ignorer la première ligne
      while(!lignes.isEmpty){
        val ligne = lignes.next().split(",")
        machines += new Machine(id, ligne(0), ligne(1).toInt, ligne(2).toInt, ligne(3).toInt)

        println(f"\nMachine $id chargée : \n   ID : $id\n  Code PIN : ${ligne(0)} \n   Lait : ${ligne(1)}mL \n   Sucre : ${ligne(2)}g \n   Café : ${ligne(3)}g")
        id += 1
      }
      println(s"${machines.length} machine(s) chargée(s) avec succès.")
    } catch {
      case _: FileNotFoundException =>
        println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        System.exit(1)
      case e: Exception =>
        println(s"Erreur inattendue : ${e.getMessage}")
        System.exit(1)
    }
    return machines
  }

  def main(args: Array[String]): Unit = {
    var machines = loadcsv("machines.csv")
    var fin = false
    var machineId = -1
    try{
      while (!fin) {
        var menu = choixMenu()
        if(menu != 3){
          machineId = choixMachine(machines.length)
          println(f"\nBienvenue sur la machine ${machines(machineId).id}.")
        }
        if (menu == 1) { // Mode Client
          machines(machineId).serveClient()
        } else if (menu == 2) { // Mode Admin
          if(!machines(machineId).validatePin()) System.exit(0) // Quitter si le code PIN est incorrect 3x
          var admin = choixAdmin()
          if(admin == 1) machines(machineId).updatePin()
          else if(admin == 2) machines(machineId).restockMachine()
        } else if (menu == 3) { // Quitter
          println("Merci d'avoir utilisé Nospresso Café. À bientôt !")
          savecsv("machines.csv", machines)
          fin = true
        } else {
          println("Choix invalide. Veuillez réessayer.")
        }
      }

    }catch{
      case _=> println(f"Erreur : Une erreur inattendue s'est produite. Veuillez réessayer.")
    }
  }

}