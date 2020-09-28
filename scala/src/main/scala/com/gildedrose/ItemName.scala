package com.gildedrose

sealed trait ItemName

object ItemName {

  final case object AgedBrie extends ItemName

  final case object Backstagepass extends ItemName

  final case object Sulfuras extends ItemName

  final case object Conjured extends ItemName

  final case object Other extends ItemName

  def itemName(name: String): ItemName = name match {
    case "Aged Brie" => AgedBrie
    case "Backstage passes to a TAFKAL80ETC concert" => Backstagepass
    case "Sulfuras, Hand of Ragnaros" => Sulfuras
    case "Conjured" => Conjured
    case _ => Other
  }

}


