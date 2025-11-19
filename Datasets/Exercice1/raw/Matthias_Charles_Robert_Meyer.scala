import io.StdIn._
import scala.util.Random
//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
object Main {
  def main(args: Array[String]): Unit = {
    var stock_cafe = 50
    var stock_sucre = 30
    var stock_lait = 0.5
    var mode = 0
    val code_pin = 434343
    do{
      mode = 0
      var test = false
      do{
        println("        Nospresso Café")
        println("Veuillez sélectionner votre mode : ")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print(">")
        mode = readInt()
        if(mode > 0 && mode < 4){
          test = true
        }else{
          println("Choix Incorrect !")
        }
      }while(!test)
      if(mode == 1) {
        var boisson = 0
        test = false
        do {
          println("Veuillez sélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print(">")
          boisson = readInt()
          if (boisson > 0 && boisson < 4) {
            test = true
          } else {
            println("Choix Incorrect !")
          }
        } while (!test)
        var taille = 0
        if(boisson == 3){
          test = false
          do{
            println("Quelle taille voulez-vous ?")
            println("1) Petit")
            println("2) Moyen")
            println("3) Grand")
            print(">")
            taille = readInt()
            if(taille > 0 && taille < 4){
              test = true
            }else{
              println("Choix Incorrect !")
            }
          }while(!test)
        }
        var sucre = 0
        test = false
        do {
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print(">")
          sucre = readInt()
          if (sucre > 0 && sucre < 5) {
            test = true
          } else {
            println("Choix Incorrect !")
          }
        } while (!test)
        var doses = 0
        if (boisson == 2 || boisson == 3) {
          var lait = 0
          test = false
          do {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            print(">")
            lait = readInt()
            if (lait == 1 || lait == 2) {
              test = true
            } else {
              println("Choix Incorrect !")
            }
          } while (!test)
          if (lait == 1) {
            test = false
            do {
              println("Combien de dose ?")
              print(">")
              doses = readInt()
              if (doses > 0 && doses < 4) {
                test = true
              } else {
                println("Choix Incorrect")
              }
            } while (!test)
          }
        }
        // VERIFICATION DES STOCKS
        var cafe_use = 0
        var sucre_use = 0
        var lait_use = 0.0
        var prix = 0.0
        if(boisson == 1){
          cafe_use = 8
          prix =  2
        }else if(boisson == 2){
          cafe_use = 6
          lait_use = 0.1
          prix = 2.5
        }else if(taille == 1){
          cafe_use = 6
          lait_use = 0.12
          prix = 2.7
        }else if(taille == 2){
          cafe_use = 8
          lait_use = 0.15
          prix = 3.2
        }else{
          cafe_use = 12
          lait_use = 0.2
          prix = 3.7
        }
        if(sucre == 2){
          sucre_use = 5
          prix = prix + 0.1
        }else if(sucre == 3){
          sucre_use = 10
          prix = prix + 0.2
        }else if(sucre == 4){
          sucre_use = 15
          prix = prix + 0.3
        }
        lait_use = lait_use + (0.05 * doses)
        var stock_ok = false
        if(cafe_use <= stock_cafe){
          stock_cafe = stock_cafe - cafe_use
          if(lait_use <= stock_lait){
            stock_lait = stock_lait - lait_use
            if(sucre_use <= stock_sucre){
              stock_sucre = stock_sucre - sucre_use
              stock_ok = true
            }else{
              println("Erreur : Quantité de sucre insuffisante pour npréparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
          }else{
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
        }else{
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if(stock_ok){
          printf("Prix total : %.2f CHF\n",prix)
          println("Veuillez payer en utilisant TWINT.")
          var code_paiement = Random.alphanumeric.take(5).mkString
          println("Votre code de paiement est : "+code_paiement)
          println("(en attente de paiement...)")
          Thread.sleep(3000)
          println("\nPaiement confirmé !")
          println("Préparation de votre boisson ...")
          if(boisson == 1){
            println("Votre Expresso est prêt ! Bonne dégustation !")
          }else if(boisson == 2){
            println("Votre Cappuccino est prêt ! Bonne dégustation !")
          }else{
            println("Votre Latte est prêt ! Bonne dégustation !")
          }
        }
      }else if(mode==2){
        println("Mode Admin :")
        var code = 0
        do {
          print("Veuillez entrer le code pin : ")
          code = readInt()
          if(code == code_pin){
            println("Accès Autorisé !")
            println("\nStocks :")
            println("Poudre de café : "+stock_cafe+"g")
            println("Lait : "+stock_lait+"L")
            println("Sucre : "+stock_sucre+"g")
            println("\nRéapprovisionnement des stocks...")
            println("Ajout:")
            var add_cafe = 0
            var add_lait = 0.0
            var add_sucre = 0
            do{
              print("Poudre de café : ")
              add_cafe = readInt()
            }while(add_cafe < 0)
            stock_cafe = stock_cafe + add_cafe
            do{
              print("Lait : ")
              add_lait = readDouble()
            }while(add_lait < 0.0)
            stock_lait = stock_lait + add_lait
            do{
              print("Sucre : ")
              add_sucre = readInt()
            }while(add_sucre < 0)
            stock_sucre = stock_sucre + add_sucre
            println("Niveaux de stocks mis a jour !")
            println("Retour au menu principal...")
          }else{
            println("Code incorrect !")
          }
        }while(code != code_pin)
      }
    }while(mode != 3)
  }
}