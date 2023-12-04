data =: 0 : 0
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
)

NB. data =: freads 'aoc23_01.txt'

NB. Возвращает битовую маску позиций строки, где единицы соответствуют
NB. десятичным цифрам.
FindDigits =: (16b39&>:*16b30&<:) @ (a.&i.)

NB. Берет первую и последнюю цифру из строки и возвращает
NB. соотвествующее этим цифрам двузначное число.
ParseFirstLast =: ". @ ({.,{:) @ (FindDigits#])

Solution1 =: +/ @ (ParseFirstLast ;. _2)

Solution1 data
