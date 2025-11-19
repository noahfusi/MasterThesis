import io.StdIn._
object Main {
  var running = true
  val nbMachines = 5
  var dose_sucre_a_soustraire = 0
  var dose_lait_a_soustraire = 0
  var dose_cafe_a_soustraire = 0
  val machinePins = Array.fill(nbMachines)("434343")
  val coffeeStocks = Array.fill(nbMachines)(50)
  val sugarStocks = Array.fill(nbMachines)(30)
  val milkStocks = Array.fill(nbMachines)(500)
  def validatePin(machineId: Int, machinePins: Array[String]) : Boolean = {
    var nb_tentatives = 3
    var pincode = readLine("Veuillez saisir le PIN :\n> ")
     while(nb_tentatives > 1 && pincode != machinePins(machineId)){
       nb_tentatives -= 1
       pincode = readLine("Nombre de tentatives restantes : " + nb_tentatives + "\nVeuillez saisir le PIN correct :\n> ")
     }
    if(pincode != machinePins(machineId)){
      return false
    } else {
      println("Accès accordé à la machine " + (machineId + 1))
      return true
    }
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var nouveau_code = readLine("Veuillez saisir le nouveau code :\n> ")
    while(nouveau_code.length != 6 || !nouveau_code.forall(_.isDigit)){
      nouveau_code = readLine("Votre code doit contenir exactement 6 chiffres :\n> ")
    } ;machinePins(machineId) = nouveau_code
    println("Code mis à jour avec succès!")
  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
      val lettre_et_chiffres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var code_Twint = ""
      var response_sucre = " "
      var supplement_lait = 0
      var nb_dose_lait = 0
      var reponse_lait = "Non"
      var taille_latte = 0
      var reponse_boisson = " "
      var prix_boisson = 0.0
      for (i <- 1 to 5) {
        val index = (math.random * 36).toInt
        var j = 0
        var symbole = ' '
        for (character <- lettre_et_chiffres) {
          if (j == index) {
            symbole = character
          }; j += 1
        }; code_Twint += symbole
      }
      var boisson = readLine("\nVeuillez sélectionner votre boisson : \n1) Expresso - 2 CHF \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
      while (boisson != 1 && boisson != 2 && boisson != 3) {
        boisson = readLine("Veuillez choisir une boisson existante : \n> ").toInt
      }
      if(boisson == 3) {
        taille_latte = readLine("\nChoisissez la taille de votre Latte : 1) CHF 2.70 (Petit), 2) CHF 3.20 (Moyen), 3) CHF 3.70 (Grand) \n> ").toInt
        while (taille_latte != 1 && taille_latte != 2 && taille_latte != 3) {
          taille_latte = readLine("Veuillez choisir une taille adéquate : \n> ").toInt
        }
      }
      var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20 \n4) Beaucoup (15g) - CHF 0.30 \n> ").toInt
      while (sucre < 1 || sucre > 4) {
        sucre = readLine("Veuillez choisir une dose de sucre existante : \n> ").toInt }
      if (sucre == 1) {
        response_sucre = "Sans sucre"
        dose_sucre_a_soustraire = 0
      } else if (sucre == 2) {
        response_sucre = "Peu"
        dose_sucre_a_soustraire = 5
      } else if (sucre == 3) {
        response_sucre = "Moyen"
        dose_sucre_a_soustraire = 10
      } else {
        response_sucre = "Beaucoup"
        dose_sucre_a_soustraire = 15
      }
      if (boisson != 1) {
        supplement_lait = readLine("\nVoulez-vous un supplément de lait ? \n1) Oui \n2) Non \n> ").toInt
        while (supplement_lait != 2 && supplement_lait != 1) {
          supplement_lait = readLine("Veuillez effectuer un choix adéquat : \n> ").toInt
        }
        if (supplement_lait == 1) {
          reponse_lait = "Oui"
          nb_dose_lait = readLine("\nCombien de doses ? \n> ").toInt
          while (nb_dose_lait != 1 && nb_dose_lait != 2 && nb_dose_lait != 3) {
            nb_dose_lait = readLine("Vous ne pouvez pas excéder trois doses de lait ! Veuillez choisir une quantité adéquate. \n> ").toInt
          }
        }
      }
      if (boisson == 1) {
        dose_cafe_a_soustraire = 8
        prix_boisson = 2.00
        reponse_boisson = "Expresso"
        println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : " + response_sucre)
      } else if (boisson == 2) {
        dose_cafe_a_soustraire = 6
        prix_boisson = 2.50
        reponse_boisson = "Cappuccino"
        dose_lait_a_soustraire = 100 + nb_dose_lait * 50
        println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : " + response_sucre + "\nSupplément lait : " + reponse_lait)
      } else {
        reponse_boisson = "Latte"
        if (taille_latte == 1) {
          dose_cafe_a_soustraire = 6
          prix_boisson = 2.70
          dose_lait_a_soustraire = 120 + nb_dose_lait * 50
          println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : " + response_sucre + "\nSupplément lait : " + reponse_lait)
        } else if (taille_latte == 2) {
          prix_boisson = 3.20
          dose_cafe_a_soustraire = 8
          dose_lait_a_soustraire = 150 + nb_dose_lait * 50
          println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : " + response_sucre + "\nSupplément lait : " + reponse_lait)
        } else {
          prix_boisson = 3.70
          dose_cafe_a_soustraire = 12
          dose_lait_a_soustraire = 200 + nb_dose_lait * 50
          println("\nBoisson sélectionnée : Latte (Grand) \nNiveau de sucre : " + response_sucre + "\nSupplément lait : " + reponse_lait)
        }
      } ; if ((coffeeStocks(machineId) - dose_cafe_a_soustraire) < 0) {
        print("\nErreur : Quantité de poudre de café insuffisante. Veuillez choisir une autre boisson ou remplir les stocks en mode Admin. ")
      return false
      } else if ((sugarStocks(machineId) - dose_sucre_a_soustraire) < 0) {
      println(sugarStocks(machineId))
        println("\nErreur : Quantité de sucre insuffisante. Veuillez choisir une autre boisson ou remplir les stocks en mode Admin. ")
      return false
      } else if ((milkStocks(machineId) - dose_lait_a_soustraire) < 0) {
        println("\nErreur : Quantité de lait insuffisante. Veuillez choisir une autre boisson ou remplir les stocks en mode Admin. ")
      return false
      } else {
        val prix_total = prix_boisson + 0.10 * (sucre - 1) + nb_dose_lait * 0.05
        if (sucre != 1) {
          if (supplement_lait == 1) {
            printf("Prix total : %.2f  CHF + %.2f CHF + %.2f CHF = %.2f CHF \n \nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_Twint + " \n(En attente de paiement...)", prix_boisson, 0.10*(sucre - 1), nb_dose_lait * 0.05, prix_total)
            Thread.sleep(3000)
            println("\n \nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + reponse_boisson + " est prêt ! Bonne dégustation !")
          } else {
            printf("Prix total : %.2f  CHF + %.2f CHF = %.2f CHF \n \nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_Twint + " \n(En attente de paiement...)", prix_boisson, 0.10*(sucre - 1), prix_total)
            Thread.sleep(3000)
            println("\n \nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + reponse_boisson + " est prêt ! Bonne dégustation !")
          }
        } else {
          if (supplement_lait == 1) {
            printf("Prix total : %.2f  CHF + %.2f CHF = %.2f CHF \n \nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_Twint + " \n(En attente de paiement...)", prix_boisson, nb_dose_lait * 0.05, prix_total)
            Thread.sleep(3000)
            println("\n \nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + reponse_boisson + " est prêt ! Bonne dégustation !")
          } else {
            println("Prix total : " + prix_boisson + " CHF \n \nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + code_Twint + " \n(En attente de paiement...)")
            Thread.sleep(3000)
            println("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + reponse_boisson + " est prêt ! Bonne dégustation !")
          }
        }
        coffeeStocks(machineId) -= dose_cafe_a_soustraire
        sugarStocks(machineId) -= dose_sucre_a_soustraire
        milkStocks(machineId) -= dose_lait_a_soustraire
        return true
      }

  }
  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("\nNiveaux de stocks : \n \tPoudre de café : " + coffeeStocks(machineId) + "g\n \tLait : " +milkStocks(machineId)*0.001 + "L\n\tSucre : " + sugarStocks(machineId) + "g")
    println("\nEntrez les quantités à ajouter : ")
    var ajout_cafe = readLine("Café :\n> ").toInt
    while(ajout_cafe < 0){
      ajout_cafe = readLine("Vous ne pouvez pas ajouter une quantité négative :\n> ").toInt
    }
    var ajout_lait = readLine("Lait :\n> ").toDouble
    while(ajout_lait < 0){
      ajout_lait = readLine("Vous ne pouvez pas ajouter une quantité négative :\n> ").toDouble
    }
    var ajout_sucre = readLine("Sucre :\n> ").toInt
    while(ajout_sucre < 0){
      ajout_sucre = readLine("Vous ne pouvez pas ajouter une quantité négative :\n> ").toInt
    }
    print("Les stocks ont été mis à jour avec succès.\nRetour vers le menu principal...")
    coffeeStocks(machineId) += ajout_cafe
    sugarStocks(machineId) += ajout_sucre
    milkStocks(machineId) += (ajout_lait * 1000).toInt
  }
  def main(args: Array[String]): Unit = {
    while(running) {
      var mode = readLine("\n \t Nospresso Café \nVeuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt
      while (mode > 3 || mode < 1) {
        mode = readLine("Veuillez sélectionner un mode correct : \n> ").toInt
      }
      if (mode == 1) {
        var paiement_confirme = false
        while(!paiement_confirme){
          var machineSelectionnee = readLine("Veuillez sélectionner votre machine (1-5) :\n> ").toInt
          while (machineSelectionnee < 1 || machineSelectionnee > 5) {
            machineSelectionnee = readLine("Veuillez sélectionner une machine existante\n> ").toInt
          }; println("Machine sélectionnée : " + machineSelectionnee)
          val paiement = serveClient(machineSelectionnee - 1, coffeeStocks, sugarStocks, milkStocks)
          if(paiement){
            paiement_confirme = true
          }
        }
      } else if (mode == 2) {
        var machineSelectionnee = readLine("Veuillez sélectionner votre machine (1-5) :\n> ").toInt
        while (machineSelectionnee < 1 || machineSelectionnee > 5) {
          machineSelectionnee = readLine("Veuillez sélectionner une machine existante\n> ").toInt
        }; println("Machine sélectionnée : " + machineSelectionnee)
        val acces_mode_admin = validatePin(machineSelectionnee - 1, machinePins)
        if(!acces_mode_admin){
          println("Trop de tentatives échouées. Fin du programme.")
          running = false
        } else {
          var gestion_machine = readLine("Que souhaitez-vous faire ?\n1) Changer le PIN\n2) Remplir les stocks\n> ").toInt
          while(gestion_machine != 1 && gestion_machine != 2){
            gestion_machine = readLine("Veuillez effectuer un choix adéquat :\n1) Changer le PIN\n2) Remplir les stocks\n> ").toInt
          }
          if(gestion_machine == 1){
            updatePin(machineSelectionnee - 1, machinePins)
          } else {
            restockMachine(machineSelectionnee - 1, coffeeStocks, sugarStocks, milkStocks)
          }
        }
      } else {
        running = false
      }
    }
  }
}