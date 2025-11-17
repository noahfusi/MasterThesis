object Main {
  import scala.util.Random
  import io.StdIn
  def main(args: Array[String]): Unit = {
    // Variables
    var Iteration = 0
    var Qcafe = 50.0
    var Qsucre = 30.0
    var Qlait = 500.0

    var PlusCafe = 0.0
    var PlusSucre = 0.0
    var PlusLait = 0.0

    var Latiteration = 0
    var AjSucre = 0
    var status = 0
    var ChoixDuCafe = 0
    var TypeDeBoisson = ""
    var DLait = ""
    var nDLait = 0

    var StringSucre = ""

    var Prix:Double = 0
    var PSucre: Double = 0
    var PLait:Double = 0
    var ILatte = 0
    var PrixFinal = 0.0

    var UseCafe = 0
    var UseSucre = 0
    var UseLait = 0

    var CodePourADMIN = 434343
    var CodePourTWINT = ""
    val Lettres_Chiffres= "1234567890ABCDEFGHIJKLMNOPQRSTUVWZ"
    var EntreeCode = 0



    while(Iteration == 0){      // Boucle du menu principal
      println("     Nospresso Cafe")
      print("1) Client   \n")
      println("2) Admin    ")
      print("3) Quitter  \n")
      print("> ")
      Iteration = StdIn.readInt()
      while ((Iteration != 1) && (Iteration != 2) && (Iteration != 3)){ // Verification des entrées
        println("Entrée incorrecte. réessayer")
        Iteration = StdIn.readInt()
        }
      }

      if(Iteration == 2){ // Mode ADMIN
        // ADMIN
        println("Mode Admin")
        println("Entrez le code PIN : ")
        while(EntreeCode != CodePourADMIN){  // Verification du mot de passe
          EntreeCode = StdIn.readInt()
          if(EntreeCode != CodePourADMIN){
            println("Code incorrect. Veuillez réeessayer.")
            print("> ")
          }
        }

        print("Accès autorisé.\n") // Montrer le stock puis le mettre a jour avec montants perso - /Affichage en Litres mais variable stock de lait en mL/
        print(f"Stocks:Poudre de cafe: $Qcafe%.2f g    \nLait          : ${Qlait*0.001}%.2f L   \nSucre         :   $Qsucre%.2f g\n")
        print("Reapprovisionnement des stocks...\n")

        println("Quantité de poudre à ajouter : ")
        PlusCafe  = StdIn.readDouble()

        println("Quantité de Sucre à ajouter : ")
        PlusSucre  = StdIn.readDouble()

        println("Quantité de lait à ajouter (en L) : ")
        PlusLait  = StdIn.readDouble()

        println("Ajout :")
        println("Poudre de cafe : " + PlusCafe + "g")
        println("Lait           : " + PlusLait + "L")
        println("Sucre          : " + PlusSucre + "g")

        Qcafe = Qcafe + PlusCafe
        Qsucre = Qsucre + PlusSucre
        Qlait = Qlait + PlusLait*1000

        println("Niveaux de stock mis a jour. ")
        println("Retour au menu principal.    " )

        EntreeCode = 0
        Iteration = 0

      } else if (Iteration == 1){ // Mode Client
        // CLIENT
        while(status == 0){

          println("Veuillez selectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")
          while(ChoixDuCafe < 1 || ChoixDuCafe > 3){ // Verification des entrées
            ChoixDuCafe = StdIn.readInt()
            if(ChoixDuCafe < 1 || ChoixDuCafe > 3){
              println("Entrée incorrecte.")
            }
          }

          if(ChoixDuCafe == 1){ // Traitement du choix de la boisson
            TypeDeBoisson = "Expresso"
            UseCafe = 8
            Prix = 2
          } else if (ChoixDuCafe == 2){
            TypeDeBoisson = "Capuccino"
            Prix = 2.5
            UseCafe = 6
            UseLait = 100
          } else if (ChoixDuCafe == 3){
            ILatte = 0
            println("Quelle taille ? ")
            println("1) Petit - CHF 2.70 ")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70 ")
            print("> ")
            while(ILatte < 1 || ILatte > 3){
              ILatte = StdIn.readInt()
              if(ILatte < 1 || ILatte > 3){
                println("Entrée incorrecte.")
              }
            }

            if(ILatte == 1){ // Traitement taille du Latte
              TypeDeBoisson = "Latte (petit)"
              Prix = 2.7
              UseCafe = 6
              UseLait = 120
            } else if (ILatte == 2){
              TypeDeBoisson = "Latte (moyen)"
              Prix = 3.2
              UseCafe = 8
              UseLait = 150
            } else if (ILatte == 3){
              TypeDeBoisson = "Latte (grand)"
              Prix = 3.7
              UseCafe = 12
              UseLait = 200
            }
          }

          println("Souhaitez-vous ajouter du sucre ?   ") // Traitement du sucre
          println("1) Sans sucre                 ")
          println("2) Peu (5g) - CHF 0.10        ")
          println("3) Moyen (10g) - CHF 0.20     ")
          println("4) Beaucoup (15g) - CHF 0.30  ")
          print("> ")
          while(AjSucre < 1 || AjSucre > 4){ // Verification des entrées
            AjSucre = StdIn.readInt()
            if(AjSucre < 1 || AjSucre > 4){
              println("Entrée incorrecte.")
            }
          }
          if (AjSucre == 1) {
            StringSucre = "Sans sucre"
            UseSucre = 0
            PSucre = 0
          } else if (AjSucre == 2) {
            StringSucre = "Peu (5g)"
            UseSucre = 5
            PSucre = 0.1
          } else if (AjSucre == 3) {
            StringSucre = "Moyen (10g)"
            UseSucre = 10
            PSucre = 0.2
          } else if (AjSucre == 4) {
            StringSucre = "Beaucoup (15g)"
            UseSucre = 15
            PSucre = 0.3
          }

          if (TypeDeBoisson != "Expresso") { // Doses de lait
            println("Souhaitez-vous ajouter du lait en supplement ?      ")
            println("(Disponible uniquement pour Capuccino et Latte   ")
            println("1) Oui")
            println("2) Non")
            print("> ")
            Latiteration = StdIn.readInt()
            while(Latiteration < 1 || Latiteration > 2){
              println("Entrée invalide.")
              Latiteration = StdIn.readInt()

            }

            if (Latiteration == 1) {
              DLait = "Oui"
              println("Combien de dose ?    ")
              print("> ")
              nDLait = StdIn.readInt()

              while (nDLait < 1 || nDLait > 3) { // Verification des entrées
                println("Veuillez selectionner une valeur correcte. ")
                print("> ")
                nDLait = StdIn.readInt()
              }

              PLait = nDLait * 0.05
              UseLait += nDLait * 50

              nDLait = 0

            } else {
              DLait = "Non"
            }
          }

          status = 1
          //Verification s'il n'y a pas d'erreurs
          if(Qcafe < UseCafe){
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée.")
            status = 0
            PlusCafe = 0
            PlusSucre = 0
            PlusLait = 0
            ChoixDuCafe = 0
            Latiteration = 0
            AjSucre = 0
            status = 0
            nDLait = 0

            UseCafe = 0
            UseSucre = 0
            UseLait = 0

            Prix = 0
            PSucre = 0
            PLait = 0
          }
          if(Qlait < UseLait){
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson selectionnée.")
            status = 0
            PlusCafe = 0
            PlusSucre = 0
            PlusLait = 0
            ChoixDuCafe = 0
            Latiteration = 0
            AjSucre = 0
            status = 0
            nDLait = 0

            UseCafe = 0
            UseSucre = 0
            UseLait = 0

            Prix = 0
            PSucre = 0
            PLait = 0
          }
          if(Qsucre < UseSucre){
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson selectionnée.")
            status = 0
            PlusCafe = 0
            PlusSucre = 0
            PlusLait = 0
            ChoixDuCafe = 0
            Latiteration = 0
            AjSucre = 0
            status = 0
            nDLait = 0

            UseCafe = 0
            UseSucre = 0
            UseLait = 0

            Prix = 0
            PSucre = 0
            PLait = 0
          }

          if(status == 0){ // Erreur et variables à 0
            println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            PlusCafe = 0
            PlusSucre = 0
            PlusLait = 0
            ChoixDuCafe = 0
            Latiteration = 0
            AjSucre = 0
            status = 0
            nDLait = 0

            UseCafe = 0
            UseSucre = 0
            UseLait = 0

            Prix = 0
            PSucre = 0
            PLait = 0
          }


        } // While status error
        if(status > 0){ // Si il n'y a pas d'erreurs, passage au payement
            for (x<-1 to 5){
              val Position = (math.random()*36).toInt
              CodePourTWINT += Lettres_Chiffres(Position)}
          println("Boisson selectionnee : " + TypeDeBoisson)
          println("Niveau de sucre : " + StringSucre)
          if (TypeDeBoisson != "Expresso") {
            println("lait supplémentaire:" + DLait)
          }

          PrixFinal = Prix + PLait + PSucre
          print(f"Prix total : $Prix%.2f  + CHF $PSucre%.2f + CHF $PLait%.2f = $PrixFinal%.2f \n")
          println("")
          println ("Veuillez payer en utilisant TWINT.")
          println( "Votre code de payement est : " + CodePourTWINT)
          println( "En attente de payement... ")

          Thread.sleep(3000)

          println("Payement confirmé.")
          println("Préparation de votre boisson...")

          Thread.sleep(5000)
          Qcafe -= UseCafe
          Qsucre -= UseSucre
          Qlait -= UseLait

          println("Votre " + TypeDeBoisson + " est prêt ! Bonne dégustation !")

          //Remettre toutes les variables de reinitialisation
          Iteration = 0

          PlusCafe = 0
          PlusSucre = 0
          PlusLait = 0
          ChoixDuCafe = 0
          Latiteration = 0
          AjSucre = 0
          status = 0
          nDLait = 0

          UseCafe = 0
          UseSucre = 0
          UseLait = 0

          Prix = 0
          PSucre = 0
          PLait = 0

        }
      } else {Iteration = -1}
    }
  }

