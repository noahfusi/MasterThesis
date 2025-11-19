import io.StdIn._
import scala.util.Random
object Main {
  def main(arg: Array[String]): Unit = {
    var pinadmin = 434343
    var PDC = 50
    var sucre = 30
    var L = 0.5
    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
    var result = ""
    var fin = true
    while(fin){
      var pin = -1
      var result = ""
      var choixmode = 0
      var choixb = 0
      var choixs = 0.0
      var choixt = 0
      var PDCC = 0
      var LC = 0.0
      var choixl = 0
      var choixx= 0
      var boisson = ""
      var niveausucre = ""
      var lait = ""
      var prixb = 0.0
      var prixs = 0.0
      var prixtotale = 0.0
      var sucreC = 0
      var x = 0
      var choixajout = 0
      var ajoutsucre = 0
      var ajoutlait = 0
      var ajoutPDC = 0
      while (choixmode != 1 && choixmode != 2 && choixmode != 3) {
        print("Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")
        choixmode = readInt()
      }
      if(choixmode == 1) {
        while (choixb != 1 && choixb != 2 && choixb != 3) {
          print("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
          choixb = readInt()
        }
        if (choixb == 1) {
          boisson = "Expresso"
        }
        if (choixb == 2) {
          boisson = "Cappuccino"
        }
        if (choixb == 3) {
          boisson = "Latte"
        }
        if (choixmode == 1) {
          while (choixs != 1 && choixs != 2 && choixs != 3 && choixs != 4) {
            print("Boisson sélectionnée :" + boisson + "\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
            choixs = readInt()
          }
        }
        if (choixs == 1) {
          niveausucre = "Sans sucre"
        }
        if (choixs == 2) {
          niveausucre = "Peu (5g)"
          sucreC += 5
        }
        if (choixs == 3) {
          niveausucre = "Moyen (10g)"
          sucreC += 10
        }
        if (choixs == 4) {
          niveausucre = "Beaucoup (15g)"
          sucreC += 15
        }
        prixs = (choixs - 1) * 0.1
        if (((choixs - 1) * 5) > sucre) {
          print("Boisson sélectionnée :" + boisson + "\nErreur : Quantité de sucre insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir moins de sucre.\n")
          choixb = 0
          choixt = 0
          PDCC = 0
          LC = 0
          x = 1
        }
        if (choixb == 3) {
          while (choixt != 1 && choixt != 2 && choixt != 3) {
            print("Boisson sélectionnée :" + boisson + "\nNiveau de sucre :" + niveausucre + "\nQuelle taille de latte voullez-vous : \n1)Petit - CHF 2.70\n2)Moyen - CHF 3.20\n3)Grand - CHF 3.70\n>")
            choixt = readInt()
          }
        }
        if(choixt == 1) {
          boisson = "Latte (Petit)"
        }
        if(choixt == 2) {
          boisson = "Latte (Moyen)"
        }
        if(choixt == 3) {
          boisson = "latte (Grand)"
        }
        if (choixb == 2 || choixb == 3) {
          while (choixx != 1 && choixx != 2) {
            print("Boisson sélectionnée :" + boisson + "\nNiveau de sucre :" + niveausucre + "\nSouhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>")
            choixx = readInt()
            if (choixx == 1) {
              while (choixl != 1 && choixl != 2 && choixl != 3) {
                print("Combien de dose ?\n>")
                choixl = readInt()
              }
            }
            if (choixx == 2) {
              lait = "non"
            }
          }
        }
        if (choixx == 1) {
          if (choixl == 1) {
            lait = "oui une dose"
          }
          if (choixl == 2) {
            lait = "oui deux dose"
          }
          if (choixl == 3) {
            lait = "oui trois dose"
          }
        }
        if (choixb == 1) {
          PDCC += 8
          prixb = 2
        }
        if (choixb == 2) {
          PDCC += 6
          LC += 0.1
          prixb = 2.50
        }
        if (choixt == 1) {
          PDCC += 6
          LC += 0.12
          prixb = 2.70
        }
        if (choixt == 2) {
          PDCC += 8
          LC += 0.15
          prixb = 3.20
        }
        if (choixt == 3) {
          PDCC += 12
          LC += 0.2
          prixb = 3.70
        }
        LC += (choixl * 0.05)
        if (PDCC > PDC) {
          print("Boisson sélectionnée :" + boisson + "\nNiveau de sucre :" + niveausucre + "\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.\n")
          x = 1
        }
        if (LC > L) {
          print("Boisson sélectionnée :" + boisson + "\nNiveau de sucre :" + niveausucre + "\nLait en supplément:" + lait + "\nErreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson.\n")
          x = 1
        }
        for (_ <- 1 to 5) {
          val randomIndex = (Math.random() * chars.length).toInt
          result += chars(randomIndex)
        }
        if (x == 0){
          prixtotale = prixs + prixb
          print("Boisson sélectionnée : " + boisson + "\nNiveau de sucre : " + niveausucre + "\nLait en supplément: " + lait + "\nPrix total : CHF " + prixb + " + CHF " + prixs + " = CHF " + prixtotale + "\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + result + "\n(En attente de paiement...)")
          Thread.sleep(3000)
          print("\nPaiement confirmé.\nPréparation de votre boisson...\nVotre " + boisson + " est prêt ! Bonne dégustation !\n")
          PDC -= PDCC
          L -= LC
          sucre -= sucreC
        }
      }
      if(choixmode == 2){
        print("Mode Admin\nEntrez le code PIN :\n>")
        pin = readInt()
      }
      if(pin == pinadmin) {
        print("Accès autorisé.")
        while (choixajout != 1 && choixajout != 2 && choixajout != 3) {
          print("Stocks:\nPoudre de café: " + PDC + "g\nLait : " + L + "L\nSucre : " + sucre + "g\nQue voullez vous ajouter?:\n1)Sucre\n2)lait\n3)Poudre de caffé\n>")
          choixajout = readInt
        }
        if(choixajout == 1){
          while (ajoutsucre <= 0){
            print("Combien de lait voulez-vous rajouter?:\n>")
            ajoutsucre = readInt()
          }
        }
        if(choixajout == 2){
          while (ajoutlait <= 0){
            print("Combien de sucre voulez-vous rajouter?:\n>")
            ajoutlait = readInt()
          }
        }
        if(choixajout == 3){
          while (ajoutPDC <= 0){
            print("Combien de poudre de lait voulez-vous rajouter?:\n>")
            ajoutPDC = readInt()
          }
        }
        PDC += ajoutPDC
        L += ajoutlait
        sucre += ajoutsucre
        print("Réapprovisionnement des stocks...\nAjout :\nPoudre de café:"+ajoutPDC+"\nLait : "+ajoutlait+"\nSucre : "+ajoutsucre+"\nNiveaux de stock mis à jour.\nRetour au menu principal...\n")
      }
      if(pin != pinadmin && pin > 0){
        print("Code Pin incorrect: Accès refusé \n")
      }
      if(choixmode == 3){
        fin = false
      }
    }
  }
}