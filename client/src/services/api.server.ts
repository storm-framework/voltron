import { UserData, User, LoginResponse, AuthInfo } from "@/types";
import router from "@/router";
import axios, { AxiosRequestConfig, AxiosResponse } from "axios";
import _ from "lodash";

const API_URL = "/api";

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

class ApiService {
  constructor(private accessToken: string | null) {}

  get sessionAccessToken(): string | null {
    return this.accessToken;
  }

  // Auth
  async signIn(info: AuthInfo): Promise<LoginResponse> {
    await delay();
    const response: AxiosResponse<LoginResponse> = await axios.post(`${API_URL}/signin`, {
      emailAddress: info.emailAddress,
      password: info.password
    });

    console.log("server-signIn", response.data);
    if (response.data.accessToken) {
      localStorage.setItem("accessToken", response.data.accessToken);
      console.log("set-access-token", response.data.accessToken);
      this.accessToken = response.data.accessToken;

    }
    return response.data;
  }

  isSignedIn() : boolean {
    const res = this.accessToken !== null;
    console.log("isSignedIn", res, this.accessToken);
    return res; 
  }

  signOut() {
    console.log("signing-out!");
    this.accessToken = null;
    localStorage.removeItem("accessToken");
    console.log("signing-out!", localStorage.getItem("accessToken"));
    return Promise.resolve();
  }

  user(token: string): Promise<UserData> {
    const payload = _.split(token, ".")[1];
    const userId = JSON.parse(atob(payload)).sub;
    return this.get(`/user/${userId}`);
  }

  async unauthorized() {
    await this.signOut();
    router.replace({ name: "Login" });
  }

  authHeader() {
    if (this.accessToken) {
      return { Authorization: "Bearer " + this.accessToken };
    } else {
      return {};
    }
  }

  async get(path: string, config?: AxiosRequestConfig): Promise<any> {
    await delay();
    try {
      const response = await axios.get(`${API_URL}${path}`, {
        headers: this.authHeader(),
        ...config
      });
      return response.data;
    } catch (error) {
      if (error?.response?.status == 401) {
        await this.unauthorized();
      }
      throw error;
    }
  }
}

const accessToken = localStorage.getItem("accessToken");

export default new ApiService(accessToken);
