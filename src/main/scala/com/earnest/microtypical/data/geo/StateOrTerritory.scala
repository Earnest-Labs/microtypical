package com.earnest.microtypical.data.geo

import com.earnest.microtypical.data.Validated
import com.earnest.microtypical.data.validation.EnumCompanionVerbose

sealed trait StateOrTerritory extends Validated

object StateOrTerritory extends EnumCompanionVerbose [StateOrTerritory] {
  case object Alabama extends StateOrTerritory
  case object Alaska extends StateOrTerritory
  case object AmericanSamoa extends StateOrTerritory
  case object Arizona extends StateOrTerritory
  case object Arkansas extends StateOrTerritory
  case object California extends StateOrTerritory
  case object Colorado extends StateOrTerritory
  case object Connecticut extends StateOrTerritory
  case object Delaware extends StateOrTerritory
  case object DistrictOfColumbia extends StateOrTerritory
  case object Florida extends StateOrTerritory
  case object Georgia extends StateOrTerritory
  case object Guam extends StateOrTerritory
  case object Hawaii extends StateOrTerritory
  case object Idaho extends StateOrTerritory
  case object Illinois extends StateOrTerritory
  case object Indiana extends StateOrTerritory
  case object Iowa extends StateOrTerritory
  case object Kansas extends StateOrTerritory
  case object Kentucky extends StateOrTerritory
  case object Louisiana extends StateOrTerritory
  case object Maine extends StateOrTerritory
  case object Maryland extends StateOrTerritory
  case object MarshallIslands extends StateOrTerritory
  case object Massachusetts extends StateOrTerritory
  case object Michigan extends StateOrTerritory
  case object Micronesia extends StateOrTerritory
  case object Minnesota extends StateOrTerritory
  case object Mississippi extends StateOrTerritory
  case object Missouri extends StateOrTerritory
  case object Montana extends StateOrTerritory
  case object Nebraska extends StateOrTerritory
  case object Nevada extends StateOrTerritory
  case object NewHampshire extends StateOrTerritory
  case object NewJersey extends StateOrTerritory
  case object NewMexico extends StateOrTerritory
  case object NewYork extends StateOrTerritory
  case object NorthCarolina extends StateOrTerritory
  case object NorthDakota extends StateOrTerritory
  case object NorthernMarianas extends StateOrTerritory
  case object Ohio extends StateOrTerritory
  case object Oklahoma extends StateOrTerritory
  case object Oregon extends StateOrTerritory
  case object Palau extends StateOrTerritory
  case object Pennsylvania extends StateOrTerritory
  case object PuertoRico extends StateOrTerritory
  case object RhodeIsland extends StateOrTerritory
  case object SouthCarolina extends StateOrTerritory
  case object SouthDakota extends StateOrTerritory
  case object Tennessee extends StateOrTerritory
  case object Texas extends StateOrTerritory
  case object Utah extends StateOrTerritory
  case object Vermont extends StateOrTerritory
  case object Virginia extends StateOrTerritory
  case object VirginIslands extends StateOrTerritory
  case object Washington extends StateOrTerritory
  case object WestVirginia extends StateOrTerritory
  case object Wisconsin extends StateOrTerritory
  case object Wyoming extends StateOrTerritory

  override protected def encoding: Map [StateOrTerritory, String] =
    Map (
      Alabama -> "AL",
      Alaska -> "AK",
      AmericanSamoa -> "AS",
      Arizona -> "AZ",
      Arkansas -> "AR",
      California -> "CA",
      Colorado -> "CO",
      Connecticut -> "CT",
      Delaware -> "DE",
      DistrictOfColumbia -> "DC",
      Florida -> "FL",
      Georgia -> "GA",
      Guam -> "GU",
      Hawaii -> "HI",
      Idaho -> "ID",
      Illinois -> "IL",
      Indiana -> "IN",
      Iowa -> "IA",
      Kansas -> "KS",
      Kentucky -> "KY",
      Louisiana -> "LA",
      Maine -> "ME",
      Maryland -> "MD",
      MarshallIslands -> "MH",
      Massachusetts -> "MA",
      Michigan -> "MI",
      Micronesia -> "FM",
      Minnesota -> "MN",
      Mississippi -> "MS",
      Missouri -> "MO",
      Montana -> "MT",
      Nebraska -> "NE",
      Nevada -> "NV",
      NewHampshire -> "NH",
      NewJersey -> "NJ",
      NewMexico -> "NM",
      NewYork -> "NY",
      NorthCarolina -> "NC",
      NorthDakota -> "ND",
      NorthernMarianas -> "MP",
      Ohio -> "OH",
      Oklahoma -> "OK",
      Oregon -> "OR",
      Palau -> "PW",
      Pennsylvania -> "PA",
      PuertoRico -> "PR",
      RhodeIsland -> "RI",
      SouthCarolina -> "SC",
      SouthDakota -> "SD",
      Tennessee -> "TN",
      Texas -> "TX",
      Utah -> "UT",
      Vermont -> "VT",
      Virginia -> "VA",
      VirginIslands -> "VI",
      Washington -> "WA",
      WestVirginia -> "WV",
      Wisconsin -> "WI",
      Wyoming -> "WY",
    )
}
