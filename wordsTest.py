# coding: utf-8

import time
import words_api
from words_api.rest import ApiException
from pprint import pprint


class Test(object):
    def test_details_words(self):
        # authentication setting using api key/token
        words_api.configuration.api_key['accessToken'] = 'YOUR_API_KEY'

        
        word = "lovely"  # Word
        detail = "definitions"  # Detail

        try:
            # (optional) initialize the API client with default base URL: http://petstore.swagger.io/v2
            api_client = words_api.ApiClient()
            words_api = words_api.WordsApi(api_client)

            # return <a href="#model_DetailsResponse">DetailsResponse (model)</a>
            response = words_api.details(word, detail)    

            pprint(response)

        
            # asynchronous call
            # thread = words_api.details(word, detail, callback=callback_function)

        except ApiException as e:
            print(e)

    def test_get_words(self):
        # authentication setting using api key/token
        words_api.configuration.api_key['accessToken'] = 'YOUR_API_KEY'

        
        word = "soliloquy"  # Word

        try:
            # (optional) initialize the API client with default base URL: http://petstore.swagger.io/v2
            api_client = words_api.ApiClient()
            words_api = words_api.WordsApi(api_client)

            # return <a href="#model_WordResponse">WordResponse (model)</a>
            response = words_api.get(word)    

            pprint(response)

        
            # asynchronous call
            # thread = words_api.get(word, callback=callback_function)

        except ApiException as e:
            print(e)

    def main(self):
        # Resource: Words
        print("### Calling endpoint: test_details_words")
        self.test_details_words()
        time.sleep(1)

        print("### Calling endpoint: test_get_words")
        self.test_get_words()
        time.sleep(1)

if __name__ == "__main__":
    Test().main()