object Main {
  import io.StdIn._
  import util.Random
  def main(args: Array[String]): Unit = {
    // variables below are necessary for loops and inputs
    var mode = ""; var coffee = ""; var sugar = ""; var milk = ""; var size = "" //selection 1-n
    var sort = ""; var kind = ""; var like = ""// for selections of quantities
    var quit = false; var cerr = false; var merr = false; var serr = false; //booleans used for menu and errors
    val PIN = 434343: Long //passcode
    var attempt = 0: Long

    //variables below is specifically for the stock
    //Note I have to use BigDecimal otherwise the computer fails to give accurate answers
    var cofpow = BigDecimal(50.0) // init quantity of coffee in g 50.0
    var suc = BigDecimal(30.0) //... of sugar ... 30.0
    var lait = BigDecimal(0.500) // ...of milk in L 0.500
    var qtycofsel = BigDecimal(0.0) // qty of coffee selected by user
    var qtymilsel = BigDecimal(0.0) //...milk...
    var qtyadmilsel = BigDecimal(0.0)//... additional milk...
    var qtysugsel = BigDecimal(0.0) // ... sugar...
    var midoz = 0// number of doses of milk
    val delta_s = BigDecimal(5.0) // how much sugar is given
    val delta_m = BigDecimal(0.05) // how much extra milk is given per dose
    val smal = BigDecimal(6.0) // small cup of coffee in g
    val med = BigDecimal(8.0) // medium...
    val lar = BigDecimal(12.0) // large...
    val c_vol = BigDecimal(0.1) // volume of milk for cappuccino
    val l_vol_p = BigDecimal(0.12) // ... for a small latte
    val l_vol_m = BigDecimal(0.15) // ... for a medium ...
    val l_vol_g = BigDecimal(0.2) // ... for a large...

    //variables below are specifically for pricing. Rounded up to 2 decimal places.
    // Note that this is done only for prices since you can have more specific quantities for the masses and volumes above.
    var sum = BigDecimal(0.0).setScale(2, BigDecimal.RoundingMode.HALF_UP) //cumulative cost of the order
    var part_sum = BigDecimal(0.0).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of an individual selection
    var sup_sum = BigDecimal(0.0).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of supplements
    val exp = BigDecimal(2.00).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of expresso in CHF
    val cap = BigDecimal(2.50).setScale(2, BigDecimal.RoundingMode.HALF_UP) // ... of cappuccino
    val lat_base = BigDecimal(2.70).setScale(2, BigDecimal.RoundingMode.HALF_UP) // ... base cost of latte
    val inc = BigDecimal(0.5).setScale(2, BigDecimal.RoundingMode.HALF_UP) //... increase in cost of latte per increase in size of latte
    val sug_base = BigDecimal(0.1).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of a single sugar dose
    val sup_milk = BigDecimal(0.05).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of a supplementary milk dose

    while (!(mode=="1"||mode=="2"||mode=="3") || !quit){ //mode selection
      val rnd = Random.alphanumeric.take(5).mkString("").toUpperCase() // twint code is done by generating 5 alphanumeric chars and then making them into an uppercase sting
      print("\nNospresso Cafe" +
        "\nVeuillez sélectionner votre mode :" +
        "\n1) Client" +
        "\n2) Admin" +
        "\n3) Quitter" +
        "\n>")
      mode = readLine()
      if(mode=="1") {
        while (!(coffee == "1" || coffee == "2" || coffee == "3")) { //coffee selection
          print("\nVeuillez sélectionner votre boisson :" +
            "\n1) Expresso - CHF " + exp +
            "\n2) Cappuccino - CHF " + cap +
            "\n3) Latte - CHF " + lat_base + " (Petit), CHF " + (lat_base + inc) + " (Moyen), CHF " + (lat_base + 2 * inc) + " (Grand)" +
            "\n>")
          coffee = readLine()
        }
        if (coffee == "1") {
          if(cofpow>=med) { // if there is more coffee than what the drink requires, then we make it. If not we throw an error
            sort = "Expresso"
            qtycofsel = med
            cofpow -= qtycofsel
            part_sum = exp
            sum += part_sum
          }else cerr=true
        }
        else if (coffee == "2") {
          if(lait>=c_vol){ //same logic as above with coffee, but now with both milk and coffee
            if(cofpow>=smal){
              sort = "Cappuccino"
              qtycofsel = smal
              cofpow -= qtycofsel
              part_sum = cap
              qtymilsel = c_vol
              lait -= qtymilsel
              sum += part_sum
            }else cerr= true
          }else merr = true
        }
        else if (coffee == "3") { //size of latte selection
          while (!(size == "1" || size == "2" || size == "3")) {
            print("\nVeuillez sélectionner la taille de votre café latte :" +
              "\n1)  (Petit), CHF " + lat_base +
              "\n2)  (Moyen), CHF " + (lat_base + inc) +
              "\n3)  (Grand), CHF " + (lat_base + 2 * inc) +
              "\n>")
            size = readLine()
          }
          if (size == "1") {
            if(lait>=l_vol_p){
              if(cofpow>=smal){
                sort = "Latte (Petit)"
                qtycofsel = smal
                cofpow -= qtycofsel
                qtymilsel = l_vol_p
                lait -= qtymilsel
                part_sum = lat_base
                sum += part_sum
              } else cerr = true
            } else merr = true
          }
          else if (size == "2") {
            if(lait>=l_vol_m){
              if(cofpow>=med){
                sort = "Latte (Moyen)"
                qtycofsel = med
                cofpow -= qtycofsel
                qtymilsel = l_vol_m
                lait -= qtymilsel
                part_sum = lat_base + inc
                sum += part_sum
              } else cerr = true
            } else merr = true
          }
          else if (size == "3") {
            if(lait>=l_vol_g){
              if(cofpow>=lar){
                sort = "Latte (Grand)"
                qtycofsel = lar
                cofpow -= qtycofsel
                qtymilsel = l_vol_g
                lait -= qtymilsel
                part_sum = lat_base + 2 * inc
                sum += part_sum
              } else cerr = true
            } else merr = true
          }
        }

        while (!(sugar == "1" || sugar == "2" || sugar == "3" || sugar == "4")) { //amount of sugar selection
          print("\nSouhaitez-vous ajouter du sucre ?" +
            "\n1) Sans sucre" +
            "\n2) Peu (5g) - CHF " + sug_base +
            "\n3) Moyen (10g) - CHF " + (2 * sug_base) +
            "\n4) Beaucoup (15g) - CHF " + (3 * sug_base)  + //gave a weird double not what expected
            "\n>")
          sugar = readLine()
        }
        if (sugar == "1" && !cerr && !merr){
          kind = "Sans sucre"
          qtysugsel = 0.0
        }
        else if (sugar == "2" && !cerr && !merr) {
          kind = "Peu (5g)"
          if(suc>=delta_s) {
            qtysugsel = delta_s
            suc -= qtysugsel
            sup_sum = sug_base
            sum += sup_sum
          } else serr = true
        }
        else if (sugar == "3" && !cerr && !merr) {
          kind = "Moyen (10g)"
          if(suc >= 2 * delta_s) {
            qtysugsel = 2 * delta_s
            suc -= qtysugsel
            sup_sum = 2 * sug_base
            sum += sup_sum
          } else serr = true
        }
        else if (sugar == "4" && !cerr && !merr) {
          kind = "Beaucoup (15g)"
          if(3 * delta_s <= suc) {
            qtysugsel = 3 * delta_s
            suc -= qtysugsel
            sup_sum = 3 * sug_base
            sum += sup_sum
          }else serr = true
        }

        if (coffee != "1") { //milk selection
          while (!(milk == "1" || milk == "2")) {
            print("Souhaitez-vous ajouter du lait en supplément ?" +
              "\n1) Oui" +
              "\n2) Non" +
              "\n>")
            milk = readLine()
          }
          if (milk == "1") {
            like = "Oui"
            do {
              print("Combien de doses de lait souhaitez-vous ? (au plus 3) : ")
              midoz = readInt()
            }while(midoz>3 || midoz<0)
              if (delta_m * midoz <= lait) {
                qtyadmilsel = midoz * delta_m
                lait -= qtyadmilsel
                sum += midoz * sup_milk
              } else merr = true
          }
          else if (milk == "2"){
            like = "Non"
            qtyadmilsel = 0.0
          }
        }

        //Correcting for the amount if there is not enough of milk or sugar
        if(serr||merr){
          cofpow+=qtycofsel
          lait+=qtymilsel
        }
        if(serr) lait+=qtyadmilsel
        if(merr) suc+=qtysugsel

        println("Boisson sélectionnée : " + sort +
          "\nNiveau de sucre : " + kind)
        if (coffee != "1") println("Lait supplémentaire : " + like)

        //Error messages are below
        if (cerr) {
          println("\nErreur : Quantité de poudre de café insuffisante pour préparer" +
            "\nla boisson sélectionnée." +
            "Veuillez choisir une autre boisson ou vérifier les" +
            "\nstocks en mode Admin.")
        }
        if (merr) {
          println("\nErreur : Quantité de lait insuffisante pour préparer" +
            "\nla boisson sélectionnée." +
            "\nVeuillez choisir une taille plus petite ou essayer" +
            "\nune autre boisson.")
        }
        if (serr) {
          println("\nErreur : Quantité de sucre insuffisant pour préparer" +
            "\nla boisson sélectionnée." +
            "Veuillez choisir une autre quantité de sucre ou vérifier les" +
            "\nstocks en mode Admin.")
        }

        //Payment process
        if (!cerr && !serr && !merr) {
          print("Prix total : CHF " + part_sum)
          if (sugar != "1") print(" + CHF " + sup_sum)
          if (milk == "1") print(" + CHF " + midoz * sup_milk)
          print(" = CHF " + sum)
          print("\n\nVeuillez payer en utilisant Twint." +
            "\nVotre code de paiement est : " + rnd +
            "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\n\nPaiement confirmé." +
            "\nPréparation de votre boisson..." +
            "\nVotre ")
            if(coffee!="3") print(sort)
            else print("Latte")
            println(" est prêt ! Bonne dégustation !")
        }
        //reset all the choices at the end of the order
        sum = 0.0; qtymilsel=0.0; qtyadmilsel=0.0; qtycofsel = 0.0; qtysugsel = 0.0; coffee = ""
        size = ""; sugar = ""; milk = ""; like = ""; kind = ""; sort = ""; cerr=false; serr=false; merr=false
      }
      else if(mode=="2") { //Admin mode
        print("\nEntrez le code PIN : ")
        attempt = readLong()

          if (attempt == PIN) {
            println("Accès autorisé.")
            attempt = 0 // so that the correct attempt is not stored
            println("\nStocks :")
            println("Poudre de café : " + cofpow + "g" +
              "\nLait : " + lait + "L" +
              "\nSucre : " + suc + "g")

            println("\nRéapprovisionnement des stocks...")
            println("Ajout :")
            print("Poudre de café : ")
            cofpow += readDouble()
            if (cofpow < 0) cofpow = 0 // you can't have negative mass in the machine...
            print("Lait: ")
            lait += readDouble()
            if (lait < 0) lait = 0 // same logic as above
            print("Sucre: ")
            suc += readDouble()
            if (suc < 0) suc = 0
            println("Niveaux de stock mise à jour.")
            println("Retour au menu principal...")
          }
      }
      else if(mode=="3") quit = true
    }
  }
}