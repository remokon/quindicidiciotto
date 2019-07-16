import tweepy

consumer_key = 'r4j6SYhTDLJsQH1i0zuLYWsVi'
consumer_secret = '0uUvAfi2XgRnOIzaouEqwcLeL4HZ0MYSmWKO1gpyBC6fwxqawD'
key = '1149429799796051969-p9aPFSriF6Jl6IVAWTicJug1w8O4Wz'
secret = '1l5QsMxpThRmb3ZzbKgtdRObxn1Gy8u42I1k4HA6LYu1W'

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(key, secret)

api = tweepy.API(auth)
m_id = api.media_upload('../quindicidiciotto/img/ultima.png')

with open('../quindicidiciotto/img/ultima.txt', 'r') as f:
    text = f.readlines()
text = ''.join(text)
api.update_status(status=text, media_ids=[m_id.media_id])
