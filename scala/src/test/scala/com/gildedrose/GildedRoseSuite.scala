package com.gildedrose

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class GildedRoseSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {
  private def check(in: Item, expected: Item) = {
    val gildedRose = new GildedRose(Array(in))
    gildedRose.updateQuality()
    val result = gildedRose.items(0)

    result.name.shouldBe(expected.name)
    result.sellIn.shouldBe(expected.sellIn)
    result.quality.shouldBe(expected.quality)
  }

  private val sellInGenerator = Arbitrary.arbitrary[Int] suchThat (i => i != Int.MinValue)
  private val qualityGenerator = Gen.choose[Int](0, 50)

  private def qualityBounded(q: Int): Int = Math.max(0, Math.min(q, 50))

  test("At the end of each day our system lowers both values for every item, the quality is never negative") {
    forAll(sellInGenerator suchThat (_ > 0), qualityGenerator) { case (sellIn, quality) =>
      check(
        new Item("normal", sellIn, quality),
        new Item("normal", sellIn - 1, qualityBounded(quality - 1))
      )
    }
  }
  test("Once the sell by date has passed, Quality degrades twice as fast") {
    forAll(sellInGenerator suchThat (_ <= 0), qualityGenerator) { case (sellIn, quality) =>
      check(
        new Item("normal", sellIn, quality),
        new Item("normal", sellIn - 1, qualityBounded(quality - 2))
      )
    }
  }
  test("\"Aged Brie\" actually increases in Quality the older it gets") {
    forAll(sellInGenerator, qualityGenerator) { case (sellIn, quality) =>
      check(
        new Item("Aged Brie", sellIn, quality),
        new Item("Aged Brie", sellIn - 1, qualityBounded(quality + 1))
      )
    }
  }
  test("\"Sulfuras\", being a legendary item, never has to be sold or decreases in Quality") {
    forAll(sellInGenerator)(sellIn => {
      check(
        new Item("Sulfuras, Hand of Ragnaros", sellIn, 80),
        new Item("Sulfuras, Hand of Ragnaros", sellIn, 80)
      )
    })
  }
  test(
    """"Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
      |	Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
      |	Quality drops to 0 after the concert""".stripMargin) {
    forAll(sellInGenerator, qualityGenerator) { case (sellIn, quality) =>
      val expectedQuality = sellIn match {
        case s if s <= 10 && s > 5 => quality + 2
        case s if s <= 5 && s > 0 => quality + 3
        case s if s <= 0 => 0
        case _ => quality - 1
      }
      check(
        new Item("Backstage passes to a TAFKAL80ETC concert", sellIn, quality),
        new Item("Backstage passes to a TAFKAL80ETC concert", sellIn - 1, qualityBounded(expectedQuality))
      )
    }
  }
  test("\"Conjured\" items degrade in Quality twice as fast as normal items") {
    forAll(sellInGenerator, qualityGenerator) { case (sellIn, quality) =>
      check(
        new Item("Conjured", sellIn, quality),
        new Item("Conjured", sellIn - 1, qualityBounded(quality - (if (sellIn > 0) 2 else 4)))
      )
    }
  }

}
