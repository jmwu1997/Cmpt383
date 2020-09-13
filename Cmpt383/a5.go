//Jia Ming Wu
//301278354

package main

import (
	"bufio"
	"errors"
	"fmt"
	"math"
	"os"
	"reflect"
	"strings"
)

func smallestDivisor(x int) int {
	if x%2 == 0 {
		return 2
	}
	for i := 3; i*i <= x; i += 2 {
		if x%i == 0 {
			return i
		}
	}
	return x
}

func isPrime(x int) bool {
	if x < 2 {
		return false
	} else {
		if smallestDivisor(x) == x {
			return true
		} else {
			return false
		}
	}
}

//idea for how to reverse an int is from https://www.geeksforgeeks.org/write-a-program-to-reverse-digits-of-a-number/
func reverse(n int) int {
	reverseInt := 0
	for {
		reverseInt = reverseInt*10 + n%10
		n = n / 10
		if n <= 0 {
			break
		}
	}
	return reverseInt
}

func countEmirpsLessThan(n int) int {
	if n < 13 {
		return 0
	} else if isPrime(n) && isPrime(reverse(n)) && n != reverse(n) {
		return countEmirpsLessThan(n-1) + 1
	} else {
		return countEmirpsLessThan(n - 1)
	}
}

func countWords(filename string) (map[string]int, error) {
	file, err := os.Open(filename)
	defer file.Close()
	if err != nil {
		return nil, errors.New("couldn't open file")
	}

	scanner := bufio.NewScanner(file)
	var words []string
	for scanner.Scan() {
		words = append(words, scanner.Text())
	}
	wordstoString := strings.Join(words, " ")
	counts := make(map[string]int)
	for _, word := range strings.Fields(wordstoString) {
		counts[word]++
	}
	return counts, nil
}

type Time24 struct {
	hour, minute, second uint8
}

//a euqal b
func equalsTime24(a Time24, b Time24) bool {
	if a.hour == b.hour && a.minute == b.minute && a.second == b.second {
		return true
	} else {
		return false
	}
}

//a before b
func lessThanTime24(a Time24, b Time24) bool {
	if a.hour < b.hour {
		return true
	} else if a.hour == b.hour && a.minute < b.minute {
		return true
	} else if a.hour == b.hour && a.minute == b.minute && a.second < b.second {
		return true
	} else {
		return false
	}
}

func (t Time24) String() string {
	return fmt.Sprintf("%02d:%02d:%02d", t.hour, t.minute, t.second)
}

func (t Time24) validTime24() bool {
	if t.hour >= 0 && t.hour < 24 && t.minute >= 0 && t.minute < 60 && t.second < 60 && t.second >= 0 {
		return true
	} else {
		return false
	}

}

func minTime24(times []Time24) (Time24, error) {
	if len(times) == 0 {
		return Time24{0, 0, 0}, errors.New("Invalid input")
	}
	minimum := Time24{23, 59, 59}
	for i := 0; i < len(times); i++ {
		if lessThanTime24(times[i], minimum) {
			minimum = times[i]
		}
	}
	return minimum, nil
}

func linearSearch(x interface{}, lst interface{}) int {
	xType := reflect.TypeOf(x)
	lstType := reflect.TypeOf(lst)
	var stringtype string
	var inttype int
	if xType != lstType.Elem() {
		panic("The input variable type are not compatible")
	}
	if xType == lstType.Elem() && reflect.TypeOf(x) == reflect.TypeOf(stringtype) {
		t := lst.([]string)
		count := -1
		for _, item := range t {
			count++
			if item == x {
				return count
			}
		}
	} else if xType == lstType.Elem() && reflect.TypeOf(x) == reflect.TypeOf(inttype) {
		t := lst.([]int)
		count := -1
		for _, item := range t {
			count++
			if item == x {
				return count
			}
		}

	}
	return -1
}

func allBitSeqs(n int) [][]int {
	emptyret := [][]int{}
	if n <= 0 {
		return emptyret
	}
	npower := math.Pow(2, float64(n))
	ret := make([][]int, int(npower))
	for i := 0; i < int(npower); i++ {
		ret[i] = generateBitarray(i, n)
	}
	return ret
}

//idea from https://stackoverflow.com/questions/8151435/integer-to-binary-array/9026235#9026235
func generateBitarray(number int, base int) []int {
	ret := make([]int, base)
	for i := 0; i < base; i++ {
		if (1<<uint(i))&number > 0 {
			ret[base-i-1] = 1
		} else {
			ret[base-i-1] = 0
		}
	}
	return ret
}

//int main
func main() {
	// fmt.Println(countEmirpsLessThan(100))
	// fmt.Println(countWords("sample.txt"))

	// fmt.Println(equalsTime24(Time24{23, 00, 01}, Time24{23, 00, 00}))

	// fmt.Println(lessThanTime24(Time24{23, 00, 01}, Time24{23, 00, 00}))
	// t := Time24{hour: 5, minute: 39, second: 8}
	// fmt.Println(t)

	// fmt.Println(minTime24([]Time24{}))
	// fmt.Println(minTime24([]Time24{Time24{01, 59, 59}, Time24{02, 59, 59}}))

	// fmt.Println(linearSearch(0, []int{4, 2, -1, 5, 0}))
	// fmt.Println(linearSearch(3, []int{4, 2, -1, 5, 0}))
	// fmt.Println(linearSearch("egg", []string{"cat", "nose", "egg"}))
	// fmt.Println(linearSearch("up", []string{"cat", "nose", "egg"}))
	// fmt.Println(allBitSeqs(0))
	// fmt.Println(allBitSeqs(1))
	// fmt.Println(allBitSeqs(2))
	// fmt.Println(allBitSeqs(3))
}
