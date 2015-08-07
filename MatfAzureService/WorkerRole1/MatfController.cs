using System;
using System.Net.Http;
using System.Web.Http;

namespace WorkerRole1
{
    public class MatfController : ApiController
    {
        public HttpResponseMessage Get()
        {
            return new HttpResponseMessage()
            {
                Content = new StringContent("Matf at your service!\nSend URL encoded LaTeX equations through GET or POST.")
            };
        }

        public HttpResponseMessage Get(string id)
        {
            string evaluation = Matf.TexRunner.evalTex(id);
            string msg = String.Format("Matf evaluation:\n{0}\n=\n{1}", id, evaluation);
            return new HttpResponseMessage()
            {
                Content = new StringContent(msg)
            };
        }

        public HttpResponseMessage Post([FromBody] string tex)
        {
            string evaluation = Matf.TexRunner.evalTex(tex);
            string msg = String.Format("Matf evaluation:\n{0}\n=\n{1}", tex, evaluation);
            return new HttpResponseMessage()
            {
                Content = new StringContent(msg)
            };
        }
    }
}