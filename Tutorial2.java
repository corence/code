
public class Tutorial2 {
    public static class City {
        public final String name;
        public final Country country;

        public City(String name, Country country) {
            this.name = name;
            this.country = country;
        }
    }

    public static class Country {
        public final String name;
        public final float averageRainfall;

        public Country(String name, float averageRainfall) {
            this.name = name;
            this.averageRainfall = averageRainfall;
        }
    }

    public static class Dude {
        public final String name;
        public final City city;

        public Dude(String name, City city) {
            this.name = name;
            this.city = city;
        }

        public String getCountryName() {
            if(this.city != null) {
                if(this.city.country != null) {
                    return this.city.country.name; // yeah, i know -- this isn't great coding style -- but it's for demo purposes, ok? :)
                }
            }
            return null;
        }

        public String getCountryNameExceptional() {
            try {
                return this.city.country.name;
            } catch(NullPointerException e) {
                return null;
            }
        }
    }
}
