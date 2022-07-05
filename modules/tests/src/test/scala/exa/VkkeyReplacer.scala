package exa

object VkkeyReplacer extends App {

  val findVkkeyRgx = """put\("([^\.]+)\.web""".r
  def replaceEText(key : String) = """put\("("""+ key+ ")"

  def replaceText(vkkeyC : String) = "put("+ vkkeyC+" + \""
  def findVkkeyCRgx(vk : String) = s"String (\\w+)\\s+= \"$vk\""

  def  replaceVkeys(cfgUtilText : String): String    = findVkkeyRgx.findFirstMatchIn(cfgUtilText).fold(cfgUtilText) {
    vkkeygroup =>
    { val vkkey = vkkeygroup.group(1)
      val replaceTextC =   findVkkeyCRgx(vkkey).r.findFirstMatchIn(mKonnektorMBeanText).map(_.group(1)).map(replaceText)
      replaceTextC.fold(cfgUtilText)(vkkeyC => {
        val configUtilTextN =   replaceEText(vkkey).r.replaceAllIn(cfgUtilText, _ => replaceTextC.get)
        replaceVkeys(configUtilTextN)
      })
    }
  }

  println(replaceVkeys(configUtilText))



  lazy val configUtilText = """
                         |                //key: vkcock
                         |                key2EinvEnum.put("vkcock.webservice.targetendpoint", VkEinvEnumKbaZugriff.VkcockWebserviceTargetendpoint);
                         |                key2EinvEnum.put("vkcock.webservice.timeout", VkEinvEnumKbaZugriff.VkcockWebserviceTimeout);
                         |                key2EinvEnum.put("vkcock.webservice.schema.version", VkEinvEnumKbaZugriff.VkcockWebserviceSchemaVersion);
                         |
                         |                //key: vkdekra
                         |                key2EinvEnum.put("vkdekra.webservice.passwort", VkEinvEnumGutachtenAbruf.DekraWebservicePasswort);
                         |                key2EinvEnum.put("vkdekra.webservice.targetendpoint", VkEinvEnumGutachtenAbruf.DekraWebserviceTargetendpoint);
                         |                key2EinvEnum.put("vkdekra.webservice.timeout", DekraWebserviceTimeout);
                         |                key2EinvEnum.put("vkdekra.webservice.useproxy", VkEinvEnumGutachtenAbruf.DekraWebserviceUseProxy);
                         |                key2EinvEnum.put("vkdekra.webservice.user", VkEinvEnumGutachtenAbruf.DekraWebserviceUser);
                         |
                         |                //key: vkdirk
                         |                key2EinvEnum.put("vkdirk.webservice.proxyhost", VkEinvEnumProxy.InternetProxyHost);
                         |                key2EinvEnum.put("vkdirk.webservice.proxypasswort", VkEinvEnumProxy.InternetProxyPasswort);
                         |                key2EinvEnum.put("vkdirk.webservice.proxyport", VkEinvEnumProxy.InternetProxyPort);
                         |                key2EinvEnum.put("vkdirk.webservice.proxyuser", VkEinvEnumProxy.InternetProxyUser);
                         |
                         |                //key: vkefak
                         |                key2EinvEnum.put("vkefak.webservice.user", VkefakWebserviceUser);
                         |                key2EinvEnum.put("vkefak.webservice.passwort", VkefakWebservicePasswort);
                         |
                         |                //key: vkeucaris
                         |                key2EinvEnum.put("vkeucaris.webservice.targetendpoint", VkeucarisWebserviceTargetendpoint);
                         |                key2EinvEnum.put("vkeucaris.webservice.timeout", VkeucarisWebserviceTimeout);
                         |                key2EinvEnum.put("vkeucaris.webservice.schema.version", VkeucarisWebserviceSchemaVersion);
                         |
                            |""".stripMargin

lazy val mKonnektorMBeanText = """    String KEY_VKKONN                         = "vkkonn";
                            |    String KEY_VKKONNDIR                      = "vkdirk";                                       //Mai 2016 eingefügt für vkgtuek: Parameter für direkte Webservice (nicht Kba)| vkonn z.B. webservice.proxyhost ->  für kba
                            |
                            |    String VKKBAKONEAR_NAME                   = "vkkbakon1.5-ear.ear";                          //201607 nötig da ZipInputStream mit Class(Loader) das ear nicht mehr ausliest; nur noch mit absolut path!
                            |
                            |    String KEY_VKZEVISK                       = "vkzevisk";
                            |
                            |    String KEY_VKEFAK                         = "vkefak";                                       //efa allgemein (für webservice user)
                            |    String KEY_VKZEVISAEK                     = "vkzevisaek";
                            |    String KEY_VKZEVISABK                     = "vkzevisabk";
                            |    String KEY_VKZFERK                        = "vkzferk";
                            |    String KEY_VKZFERMITK                     = "vkzfermitk";
                            |    String KEY_VKRESPERK                      = "vkresperk";
                            |    String KEY_VKZKRK_BESTELLUNG              = "vkzkrkb";
                            |    String KEY_VKZKRK_ANFRAGE                 = "vkzkrka";                                      // für Anfrage und (Status) Mitteilung (ZkrAnfrageMKonnektor) -> kein vkzkrk
                            |
                            |    //XKfz Gutachten: (Achtung: auch identisch in GtaVorgang!)
                            |    String KEY_VKGTUEK                        = "vkgtuek";
                            |    String KEY_VKKUES                         = "vkkues";
                            |
                            |    String KEY_VKCOCK                         = "vkcock";
                            |    //Digant - bisher kein Konnektor. Braucht aber (entpackten) Inhalt in vkkonnConf
                            |    String KEY_VKEFSR                         = "vkefsr";
                            |
                            |    //abhängig von  vkzfzr-mserver-gen (-> mit version-id KEYPREFIX_VERSION_ZFZR+"..." mit "..." aus VkkonnWebserviceSchemaVersion,  vgl. MKonnektor.PathLocation.getVersionIdentifier(String vkkey))
                            |    String KEY_VKZFZRK                        = "vkzfzrk";
                            |    String KEY_VKZFZRBK_BATCH                 = "vkzfzrbk";
                            |    //brauchen nur encryption: (ab Version 4.2 auch das übrige)
                            |    String KEY_VKSTK                          = "vkstk";
                            |    String KEY_VKSTK_RUECKSTAND               = "vkstkr";
                            |    String KEY_VKSTK_BANKDATEN                = "vkstkb";
                            |    String KEY_VKSTK_KLAR                     = "vkstkklar";
                            |    String KEY_VKSTK_RUECKSTAND_KLAR          = "vkstkrklar";
                            |    String KEY_VKSTK_BANKDATEN_KLAR           = "vkstkbklar";
                            |
                            |    //abhängig von  vksteuerfall-gen
                            |    String KEY_VKZFZRK_STEUER                 = "vkzfzrks";                                     //-> SchemaVersionSteuerEnum NICHT SchemaVersionSteuerkEnum
                            |    //braucht nur fehlernachricht :
                            |    String KEY_VKSTBK_STEUER_BATCH            = "vkstbk";                                       //ist eigentlich Zfzr_Steuer_Batch
                            |
                            |    /* für Service-Methoden per Ikfz/OK-KOMM: für Gen/Abfrage von Cert für IkfzAntraege-Bankdaten (ZertifikatAbfrageServiceImpl.getEncmap) -
                            |    und für Ikfz-Vorgänge (vgl. Kommentar ZertifikatAbfrageServiceImpl.versionIdChanged(), Ikfz3VkOkkommTestanfrageFassadeImpl.versionIdChanged)
                            |    */
                            |    String KEY_VKIKFZ                         = "vkikfz";
                            |    /*für Import Zfzr-Ikfz-Antraege:
                            |     * ab 4.2 in vkzfzr-mserver-gen mit enthalten (vgl. VkZfzrBatchServiceImpl.vkikfzbkVersionIdChanged())
                            |     *  <-> ab ikfz-stufe3 ohne ..gen, aber noch Validierung mit Zfzr-Ikfz-Antraege.xsd (vgl. IkfzImportKbaStufe3Job.verarbeiteEinzelsatz)
                            |     */
                            |    String KEY_VKIKFZBK_BATCH                 = "vkikfzbk";
                            |
                            |    String KEY_VKINFODTK                      = "vkinfodtk";
                            |    String KEY_VKZPASK                        = "vkzpas";                                       //Achtung: ohne k                      //wäre besser einheitlich mit k am Ende
                            |    String KEY_VKGDVK                         = "vkgdvk";
                            |    String KEY_VKGDVDK                        = "vkgdvdk";                                      //Achtung: MKonnektorAbstract.getConfigType = DIRECT
                            |    String KEY_VKINFOVNK                      = "vkinfovnk";
                            |    String KEY_VKVZRAB                        = "vkvzrkAb";
                            |    String KEY_VKVZRAE                        = "vkvzrkAe";
                            |    String KEY_VKEUCARIS                      = "vkeucaris";
                            |    String KEY_VKDEKRA                        = "vkdekra";
                            |    String KEY_VKTUEVRHLND                    = "vktuevrhlnd";
                            |    String KEY_VKTUEVNORD                     = "vktuevnord";
                            |    String KEY_VKTACHONETK                    = "vktachonetk";
                            |    String KEY_VKTUEVNORDFSW                  = "vktuevnordfsw";
                            |    String KEY_VKTUEVSUEDFSW                  = "vktuevsuedfsw";
                            |    String KEY_TUVSUEDFSW_SERVICE             = "BHSS_v2_Service";                              //upload-> nur!ausgehendes, download->nur!eingehendes XML
                            |    String KEY_VKFAERMITK                     = "vkfaermitk";
                            |    String KEY_VKFAERABK                      = "vkfaerabk";
                            |    String KEY_VKFAERABK_KFZ                  = "vkfaerabk" + ConfigUtil.DELIMETER4KEY + "kfz"; //(NUR) für eigene MKonnektor-Instanz: andere Kennung!!!
                            |    String KEY_VKFAERUNTK                     = "vkfaeruntk";
                            |    String KEY_VKFAERAEK                      = "vkfaeraek";
                            |    //    String                KEY_VKFAERAEK_KFZ                  = "vkfaeraekkfz";
                            |    String KEY_VKBQR                          = "vkbqr";
                            |    String KEY_VKBQR_SERVICE                  = "AnfrageStub";                                  // nur!eingehendes XML
                            |    String KEY_VKBQRMIT                       = "vkbqrmit";
                            |    String KEY_VKBQRZUSATZMIT                 = "vkbqrzusatzmit";
                            |
                            |""".stripMargin

}
