import scala.io.StdIn._
import scala.util.Random

    object main {
      val nbMachines = 5
      var machinePins = Array.fill(nbMachines)("434343")
      var coffeeStocks = Array.fill(nbMachines)(50) // 50g
      var sugarStocks = Array.fill(nbMachines)(30) // 30g
      var milkStocks = Array.fill(nbMachines)(500) // 500mL

      def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
        var essais = 3
        while(essais > 0){
          val pin = readLine("Entrez le code PIN : \n> ")
          if(pin == machinePins(machineId)){
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

      def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
        println(f"Mise à jour du code PIN pour la Machine ${machineId + 1}")
        var pin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
        while(pin.length != 6){
          pin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
        }
        machinePins(machineId) = pin
        println("Le code PIN a été mis à jour avec succès. \nRetour au menu principal...")
      }

      def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
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
        if(coffeeStocks(machineId) < cafeUtilise) println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")

        if(milkStocks(machineId) < laitUtilise) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")

        if(milkStocks(machineId) < sucreUtilise) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")

        if(coffeeStocks(machineId) >= cafeUtilise && milkStocks(machineId) >= laitUtilise && sugarStocks(machineId) >= sucreUtilise){

          println(f"Boisson sélectionée : $nomBoisson \nNiveau de sucree : $niveauSucre \nLait supplémentaire : $supplementLait\n")
          println(f"Prix total : CHF $prixBase%.2f + CHF $prixSucre%.2f + CHF $prixLait%.2f = CHF ${prixBase + prixSucre + prixLait}%.2f")

          println("\nVeuillez payer en utilisant Twint.")
          println(f"Votre code de paiement est : ${Random.alphanumeric.take(5).mkString}")
          println("(En attente de paiement...)")

          Thread.sleep(3000)

          println("\nPaiement confirmé. \nPréparation de votre boisson...")

          coffeeStocks(machineId) -= cafeUtilise
          milkStocks(machineId) -= laitUtilise
          sugarStocks(machineId) -= sucreUtilise

          Thread.sleep(3000)

          println(f"Votre $nomBoisson est prêt ! Bonne dégustation.")
          return true

        }
        else{
          println("Les stocks pour préparer votre boissons sont insuffisants. Veuillez éessayer une autre boisson ou une autre machine.")
          return false
        }
      }

      def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

        println(f"\nStocks : \n   Café: ${coffeeStocks(machineId)}g \n   Lait: ${milkStocks(machineId)}mL \n   Sucre: ${sugarStocks(machineId)}g")

        println("\nRéapprovisionnement des stocks...\nAjout:")
        val ajoutCafe = readLine("   Poudre de café > ").toInt
        val ajoutLait = readLine("   Lait > ").toInt
        val ajoutSucre = readLine("   Sucre > ").toInt

        coffeeStocks(machineId) += ajoutCafe
        milkStocks(machineId) += ajoutLait
        sugarStocks(machineId) += ajoutSucre

        println("Les stocks ont été mis à jour avec succès. \nRetour au menu principal...")

      }

      def choixMachine(): Int = {
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

      def main(args: Array[String]): Unit = {
        var fin = false
        var machineId = -1
        while (!fin) {
          var menu = choixMenu()
          if(menu != 3){
            machineId = choixMachine()
            println(f"\nBienvenue sur la machine ${machineId + 1}")
          }
          if (menu == 1) { // Mode Client
            serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
          } else if (menu == 2) { // Mode Admin
            if(!validatePin(machineId, machinePins)) System.exit(0) // Quitter si le code PIN est incorrect 3x
            var admin = choixAdmin()
            if(admin == 1) updatePin(machineId, machinePins)
            else if(admin == 2) restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
          } else if (menu == 3) { // Quitter
            println("Merci d'avoir utilisé Nospresso Café. À bientôt !")
            fin = true
          } else {
            println("Choix invalide. Veuillez réessayer.")
          }
        }
      }

}