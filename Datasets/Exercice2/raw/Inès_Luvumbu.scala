object Main {
  import io.StdIn
  import scala.util.Random
  var Mainmenu: Boolean = true
  var Input: Int = 0
  val ChoixClient: Int = 1
  val ChoixAdmin: Int = 2
  val ChoixQuitter: Int = 3
  var InputCode = ""
  var PrixBoisson = 0.0
  var BoissonChoisie = ""
  var Sugarlevel = ""
  var PriceSugar = 0.000
  var AddSugar = 0
  var AddLait = 0
  var AddPoudre = 0
  var DoseLait = 0
  var PrixLait = 0.0
  var SupLait = ""
  var taillelatte = ""
  var nerror = 0
  var Jesuicoincé = true
  var TotalPrice = 0.0

  var coffeeStocks: Array[Int] = Array(50, 50, 50, 50, 50)
  var milkStocks = Array(500,500,500,500,500)
  var sugarStocks = Array(30,30,30,30,30)

  var TWINT = ""

  var CodePIN = 434343

  var approvSucre = 0
  var approvLait = 0.0
  var approvPoudre = 0
  var n = 0
  val nbMachines = 5
  var machinePins: Array[String] = Array("434343","434343","434343","434343","434343")
  var transac = true
  var machineID = 6

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println(f"Mise a jour du PIN pour la machine $machineId.")
    print("Entrez un nouveau code PIN a 6 chiffres >")
    InputCode = StdIn.readLine()
    while(!(InputCode.forall(_.isDigit)) || InputCode.length != 6){
      println("Entrez un nouveau code PIN a 6 chiffres >")
      InputCode = StdIn.readLine()
    }
    machinePins(machineId) = InputCode
    println("Le code a ete mis a jour avec succes.\nRetour au menu principal...")
  }


  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    println("Entrez le code PIN : ")
    InputCode = StdIn.readLine()


    while(InputCode != machinePins(machineId) && n <2) { // VERIFICATION PIN
      print (f"Code PIN incorrect. ${2-n} tentatives restantes.\n> ")
      n += 1
      InputCode = StdIn.readLine()
      if(2-n == 0 && InputCode != machinePins(machineId)){
        println("Code PIN incorrect. 0 tentatives restantes.")
      }
    }
    if(InputCode != machinePins(machineId)){n = 5}
    if(n == 5){ return false} else {return true}
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {

    println("Acces autorise.")
    println   ("") // Afficher le stock puis le mettre a jour
    println   (f"Stocks: \nPoudre de cafe:  ${coffeeStocks(machineId)}%.2f g     \nLait          :   ${milkStocks(machineId)*0.001}%.2f L \nSucre         : ${sugarStocks(machineId)}%.2f g")

    print("Poudre de cafe >")
    approvPoudre = StdIn.readInt()
    coffeeStocks(machineId) += approvPoudre

    print("Lait >")
    approvLait = StdIn.readDouble()
    milkStocks(machineId) += (approvLait*1000).toInt
    print("Sucre >")
    approvSucre = StdIn.readInt()
    sugarStocks(machineId) += approvSucre

    println("Ajout :")
    println("\tPoudre de cafe : " + approvPoudre + "g")
    println("\tSucre          : " + approvSucre + "g")
    println("\tLait           : " + approvLait + "L")

    print  ("Les stocks ont été mis à jour avec succès.\n" )
    println     ("Retour au menu principal..." )

  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

    print("Veuillez selectionner votre boisson :\n")
    print("1) Expresso - CHF 2.00\n")
    print("2) Cappuccino - CHF 2.50\n")
    print("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n")
    print("> ")
    var InputClient = 0
    InputClient = StdIn.readInt()

    while (InputClient != 1 && InputClient != 2 && InputClient != 3) { // Verification valeur correcte
      print("Veuillez insérer une valeur correcte     \n> ")
      InputClient = StdIn.readInt()
    }

    if (InputClient == 1) { // Choix de l'expresso par l'utilisateur
      BoissonChoisie = "Expresso"
      PrixBoisson = 2
      AddPoudre = 8
    } else if (InputClient == 2) {
      BoissonChoisie = "Cappuccino"
      PrixBoisson = 2.5
      AddPoudre = 6
      AddLait = AddLait + 100
    } else if (InputClient == 3) {
      BoissonChoisie = "Latte"
      println("Quelle taille ?    ")
      println("1) Petit - CHF 2.70  ")
      println("2) Moyen - CHF 3.20 ")
      print("3) Grand - CHF 3.70  \n> ")
      InputClient = StdIn.readInt()

      while (InputClient < 1 || InputClient > 3) { // Verification valeur correcte
        println("Veuillez selectionner une valeur correcte.    \n> ")
        InputClient = StdIn.readInt()
      }

      if (InputClient == 1) { // Traitement  de la taile du latte
        BoissonChoisie = "Latte (Petit)"
        PrixBoisson = 2.7000
        AddPoudre = 6
        AddLait = AddLait + 120
      } else if (InputClient == 2) {
        BoissonChoisie = "Latte (Moyen)"
        PrixBoisson = 3.200
        AddPoudre = 8
        AddLait = AddLait + 150
      } else if (InputClient == 3) {
        BoissonChoisie = "Latte (Grand)"
        PrixBoisson = 3.70
        AddPoudre = 12
        AddLait = AddLait + 200
      }

    }

    println("Souhaitez-vous ajouter du sucre ?   ")
    println("1) Sans sucre       ")
    println("2) Peu (5g) - CHF 0.10    ")
    println("3) Moyen (10g) - CHF 0.20   ")
    println("4) Beaucoup (15g) - CHF 0.30  ")
    print("> ")
    InputClient = StdIn.readInt()

    while (InputClient < 1 || InputClient > 4) {
      println("Veuillez selectionner une valeur correcte.     ")
      InputClient = StdIn.readInt()
    }

    AddSugar = (InputClient - 1) * 5
    PriceSugar = (InputClient - 1) * 0.1

    if (InputClient == 1) {
      Sugarlevel = "Sans sucre"
    } else if (InputClient == 2) {
      Sugarlevel = "Peu (5g)"
    } else if (InputClient == 3) {
      Sugarlevel = "Moyen (10g)"
    } else if (InputClient == 4) {
      Sugarlevel = "Beaucoup (15g)"
    }

    if (BoissonChoisie != "Expresso") { // Traitement du lait (pas pour expresso)
      println("Souhaitez-vous ajouter du lait en supplement ?      ")
      println("(Disponible uniquement pour Capuccino et Latte   ")
      println("1) Oui        \n2) Non")
      print("> ")
      InputClient = StdIn.readInt()

      while (InputClient != 1 && InputClient != 2) {
        println("Veuillez selectionner une valeur correcte.")
        print("> ")
        InputClient = StdIn.readInt()
      }

      if (InputClient == 1) {
        SupLait = "Oui"
        println("Combien de dose ?    ")
        print("> ")
        DoseLait = StdIn.readInt()

        while (DoseLait < 1 || DoseLait > 3) {
          println("Veuillez selectionner une valeur correcte.         ")
          print("> ")
          DoseLait = StdIn.readInt()
        }

        PrixLait = DoseLait * 0.05
        AddLait = AddLait + DoseLait * 50

      } else {
        SupLait = "Non"
      }
    }

    // Erreurs
    if (coffeeStocks(machineId) < AddPoudre) {
      println("Erreur : Quantite de poudre de cafe insuffisante pour preparer la boisson selectionnee.")
      nerror += 1
      AddLait = 0
      AddSugar = 0
      AddPoudre = 0
      DoseLait = 0
      PrixLait = 0
    }

    if (sugarStocks(machineId) < AddSugar) {
      println("Erreur : Quantite de sucre insuffisante pour preparer la boisson selectionnee.")
      nerror += 1
      AddLait = 0
      AddSugar = 0
      AddPoudre = 0
      DoseLait = 0
      PrixLait = 0
    }

    if (milkStocks(machineId) < AddLait) {
      println("Erreur : Quantite de lait insuffisante pour preparer la boisson selectionnee.")
      nerror += 1
      AddLait = 0
      AddSugar = 0
      AddPoudre = 0
      DoseLait = 0
      PrixLait = 0
    }

    if (nerror == 0) { // Si pas erreur, on sort de la boucle et passe au payement
      milkStocks(machineId) -= AddLait
      sugarStocks(machineId) -= AddSugar
      coffeeStocks(machineId) -= AddPoudre

      TWINT = Random.alphanumeric.take(5).mkString.toUpperCase()
      println("Boisson selectionnee : " + BoissonChoisie)
      println("Niveau de sucre : " + Sugarlevel)
      if (BoissonChoisie != "Expresso") {
        println("lait supplémentaire:" + SupLait)
      }
      TotalPrice = PrixBoisson + PriceSugar + PrixLait
      println(f"Prix total : $PrixBoisson%.2f  + CHF $PriceSugar%.2f + CHF $PrixLait%.2f = $TotalPrice%.2f")

      print("Veuillez payer en utilisant TWINT.\n")
      println("Votre code de payement est :" + TWINT)
      println("En attente de payement...")
      Thread.sleep(3000)   // Attendre 3 secondes

      println("Payement confirmé.\nPréparation de votre boisson...")
      Thread.sleep(5000)   // Attendre 3 secondes
      println("Votre " + BoissonChoisie + " est prêt ! Bonne dégustation !")
      Input = 0
      return(true)
    }
    else {
      println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      nerror = 0
      return(false)
    }

  }


  def main(args: Array[String]): Unit = {


    while(Mainmenu) { // BOUCLE MAINMENU
      if(transac){
        print("\t Nospresso Cafe\n")
        print("1) Client\n")
        print("2) Admin\n")
        print("3) Quitter\n")
        print("> ")
        Input = StdIn.readInt()
      }

      while (1 > Input && Input > 3) { // Verification valeur correcte
        print("Selectionnez une valeur correcte.")
        print("> ")
        Input = StdIn.readInt()
      }

      if(Input != 3){
        println("Machine selectionnee : >")
        machineID = StdIn.readInt()-1
        while (0 > machineID || machineID > 4) { // Verification valeur correcte
          print("Selectionnez une valeur correcte.")
          print("> ")
          machineID = StdIn.readInt()-1
        }
      }



      if (Input == ChoixClient) {

        transac = serveClient(machineID, coffeeStocks, sugarStocks, milkStocks)

        AddLait = 0
        AddSugar = 0
        AddPoudre = 0
        DoseLait = 0
        PrixLait = 0


      }


      if (Input == 2) { // MODE ADMIN
        print("Mode Admin      \n")

        if(validatePin(machineID, machinePins) == true){
          println("1)Remplir les stocks")
          println("2)Changer PIN\n>")
          Input = StdIn.readInt()
          while (1 > Input && Input > 2) { // Verification valeur correcte
            print("Selectionnez une valeur correcte.")
            print("> ")
            Input = StdIn.readInt()
          }
          if(Input == 1){
            restockMachine(machineID, coffeeStocks, sugarStocks, milkStocks)
          } else {
            updatePin(machineID, machinePins)
          }

          Input = 0
          n = 0
        } else {
          println("Trop de tentatives échouées. Fin du programme. ")
          Input = 3
          n = 0
        }

      }

      if (Input == 3) {  // Quitter la machine
        Mainmenu = false
      }

    }
  }
}
