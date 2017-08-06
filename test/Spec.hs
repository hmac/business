import Test.Hspec
import Data.Dates
import Business

main :: IO ()
main = hspec $ do
  describe "datesBetween" $ do
    it "returns the dates between the two given dates" $
      let date1 = DateTime 2017 1 1 0 0 0
          date2 = DateTime 2017 1 3 0 0 0 in
        datesBetween date1 date2 `shouldBe` [date1, DateTime 2017 1 2 0 0 0]
    it "returns no dates when given the same date twice" $
      let date = DateTime 2017 1 1 0 0 0 in
        datesBetween date date `shouldBe` []
    it "is inclusive of the start date, but exclusive of the end date" $
      let start = DateTime 2017 1 1 0 0 0
          end = DateTime 2017 1 2 0 0 0 in
        datesBetween start end `shouldBe` [start]

  describe "isBusinessDay" $
    let monday = lastMonday $ DateTime 2017 1 1 0 0 0 in do
      it "returns True when given a working day" $
        let config = Config { workingDays = [Monday], holidays = [] } in
          isBusinessDay config monday `shouldBe` True
      it "returns False when given a non-working day" $
        let config = Config { workingDays = [Tuesday], holidays = [] } in
          isBusinessDay config monday `shouldBe` False
      it "returns False when given a working day which is also a holiday" $
        let config = Config { workingDays = [Monday], holidays = [monday] } in
          isBusinessDay config monday `shouldBe` False
      it "returns False when given a holiday which is a non-working day" $
        let config = Config { workingDays = [Tuesday], holidays = [monday] } in
          isBusinessDay config monday `shouldBe` False

  describe "rollForward" $
    let monday = lastMonday $ DateTime 2017 1 1 0 0 0
        tuesday = addInterval monday (Days 1) in do
      it "returns the given date if it is a working day" $
        let config = Config { workingDays = [Monday], holidays = [] } in
          rollForward config monday `shouldBe` monday
      it "returns the next business day if the given date isn't one" $ do
        let config = Config { workingDays = [Tuesday], holidays = [] } in
          rollForward config monday `shouldBe` tuesday
        let config = Config { workingDays = [Monday, Tuesday], holidays = [monday] } in
          rollForward config monday `shouldBe` tuesday

  describe "rollBackward" $
    let monday = lastMonday $ DateTime 2017 1 1 0 0 0
        tuesday = addInterval monday (Days 1)
        wednesday = addInterval tuesday (Days 1) in do
      it "returns the given date if it is a working day" $
        let config = Config { workingDays = [Monday], holidays = [] } in
          rollBackward config monday `shouldBe` monday
      it "returns the previous business day if the given date isn't one" $ do
        let config = Config { workingDays = [Monday], holidays = [] } in
          rollBackward config tuesday `shouldBe` monday
        let config = Config { workingDays = [Monday, Tuesday], holidays = [tuesday] } in
          rollBackward config tuesday `shouldBe` monday
        let config = Config { workingDays = [Monday, Tuesday], holidays = [tuesday] } in
          rollBackward config wednesday `shouldBe` monday

  describe "nextBusinessDay" $
    let monday = lastMonday $ DateTime 2017 1 1 0 0 0
        tuesday = addInterval monday (Days 1) in
      it "returns the next business day from the given date, even if it is a business day" $ do
        let config = Config { workingDays = [Monday, Tuesday], holidays = [] } in
          nextBusinessDay config monday `shouldBe` tuesday
        let config = Config { workingDays = [Tuesday], holidays = [] } in
          nextBusinessDay config monday `shouldBe` tuesday
        let config = Config { workingDays = [Monday, Tuesday], holidays = [monday] } in
          nextBusinessDay config monday `shouldBe` tuesday

  describe "previousBusinessDay" $
    let monday = lastMonday $ DateTime 2017 1 1 0 0 0
        tuesday = addInterval monday (Days 1) in
      it "returns the previous business day from the given date, even if it is a business day" $ do
        let config = Config { workingDays = [Monday, Tuesday], holidays = [] } in
          previousBusinessDay config tuesday `shouldBe` monday
        let config = Config { workingDays = [Monday], holidays = [] } in
          previousBusinessDay config tuesday `shouldBe` monday
        let config = Config { workingDays = [Monday, Tuesday], holidays = [tuesday] } in
          previousBusinessDay config tuesday `shouldBe` monday

  describe "addBusinessDays" $
    let monday = lastMonday $ DateTime 2017 1 1 0 0 0
        tuesday = addInterval monday (Days 1)
        wednesday = addInterval tuesday (Days 1)
        friday = addInterval wednesday (Days 2) in do
      it "adds the given number of business days to the date" $
        let config = Config { workingDays = [Monday, Tuesday, Wednesday], holidays = [] } in
          addBusinessDays config monday 1 `shouldBe` tuesday
      context "when there are non-working days or holidays in the interval" $
        it "skips them" $ do
          let config = Config { workingDays = [Monday, Wednesday], holidays = [] } in
              addBusinessDays config monday 1 `shouldBe` wednesday
          let config = Config { workingDays = [Monday, Tuesday, Wednesday, Friday], holidays = [wednesday] } in
              addBusinessDays config monday 2 `shouldBe` friday
      context "when given a negative number of days" $
        it "delegates to subtractBusinessDays, and does the expected thing" $
          let config = Config { workingDays = [Monday, Tuesday], holidays = [] } in
              addBusinessDays config tuesday (-1) `shouldBe` monday

  describe "subtractBusinessDays" $
    let monday = lastMonday $ DateTime 2017 1 1 0 0 0
        tuesday = addInterval monday (Days 1)
        wednesday = addInterval tuesday (Days 1)
        friday = addInterval wednesday (Days 2) in do
      it "subtracts the given number of business days from the date" $
        let config = Config { workingDays = [Monday, Tuesday, Wednesday], holidays = [] } in
          subtractBusinessDays config tuesday 1 `shouldBe` monday
      context "when there are non-working days or holidays in the interval" $
        it "skips them" $ do
          let config = Config { workingDays = [Monday, Wednesday], holidays = [] } in
              subtractBusinessDays config wednesday 1 `shouldBe` monday
          let config = Config { workingDays = [Monday, Tuesday, Wednesday, Friday], holidays = [wednesday] } in
              subtractBusinessDays config friday 2 `shouldBe` monday
      context "when given a negative number of days" $
        it "delegates to addBusinessDays, and does the expected thing" $
          let config = Config { workingDays = [Monday, Tuesday], holidays = [] } in
              subtractBusinessDays config monday (-1) `shouldBe` tuesday
  describe "businessDaysBetween" $
    it "returns the number of business days between the two days" $
      let monday = lastMonday $ DateTime 2017 1 1 0 0 0
          tuesday = addInterval monday (Days 1)
          wednesday = addInterval tuesday (Days 1)
          thursday = addInterval wednesday (Days 1)
          friday = addInterval thursday (Days 1) in do
        let config = Config { workingDays = [Monday, Tuesday, Wednesday, Thursday, Friday], holidays = [] } in do
            businessDaysBetween config monday friday `shouldBe` 4
            businessDaysBetween config monday (nextMonday monday) `shouldBe` 5
        let config = Config { workingDays = [Monday, Tuesday, Wednesday, Thursday, Friday], holidays = [thursday] }  in
            businessDaysBetween config monday friday `shouldBe` 3
        let config = Config { workingDays = [Monday, Tuesday, Wednesday, Thursday, Friday], holidays = [monday] }  in
            businessDaysBetween config monday tuesday `shouldBe` 0
        let config = Config { workingDays = [Monday, Wednesday, Thursday, Friday], holidays = [tuesday] } in
          businessDaysBetween config monday friday `shouldBe` 3
