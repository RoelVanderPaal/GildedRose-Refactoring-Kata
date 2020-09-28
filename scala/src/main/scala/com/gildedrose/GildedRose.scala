package com.gildedrose

import com.gildedrose.ItemName._


class GildedRose(val items: Array[Item]) {
  def updateQuality(): Unit = {
    items.foreach(item => {
      val itemName = ItemName.itemName(item.name)
      item.quality += (itemName match {
        case AgedBrie => 1
        case Backstagepass =>
          item.sellIn match {
            case i if i <= 10 && i > 5 => 2
            case i if i <= 5 && i > 0 => 3
            case i if i <= 0 => -item.quality
            case _ => -1
          }
        case Other => if (item.sellIn <= 0) -2 else -1
        case Conjured => if (item.sellIn <= 0) -4 else -2
        case Sulfuras => 0
      })
      if (itemName != Sulfuras) {
        item.sellIn -= 1
        if (item.quality > 50) {
          item.quality = 50
        }
        if (item.quality < 0) {
          item.quality = 0
        }
      }
    })
  }
}