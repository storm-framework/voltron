<template>
  <div class="reset">
    <b-form class="form-reset text-center" @submit.prevent="onSubmit">
      <br />
      <h3 class="mb-8">Reset your password</h3>
      <br />
      <b-form-input
        id="email-address"
        type="email"
        v-model="emailAddress"
        required
        placeholder="Email address"
      ></b-form-input>

      <b-form-invalid-feedback :state="isValidEmail">
        Invalid email address.
      </b-form-invalid-feedback>

      <b-button
        :disabled="isSentResetCode"
        variant="primary"
        block
        size="lg"
        type="submit"
        class="mt-4"
      >
        Get Reset Code
      </b-button>

      <br />
      <transition name="fade">
        <div v-if="isSentResetCode">
          <b-form-input
            id="reset-code"
            type="text"
            v-model="resetCode"
            required
            placeholder="Reset code (emailed from voltron.sys)"
          ></b-form-input>

          <b-form-input
            id="new-password"
            type="password"
            v-model="resetPassword"
            required
            placeholder="New password"
          ></b-form-input>

          <b-button-group>
            <b-button
              variant="outline-primary"
              :disabled="isSubmittedResetCode"
              block
              size="lg"
              class="mt-1"
              @click="resetCancel"
            >
              Cancel
            </b-button>

            <b-button
              variant="primary"
              :disabled="isSubmittedResetCode"
              block
              size="lg"
              class="mt-1"
              @click="resetPass"
            >
              Reset
            </b-button>
          </b-button-group>
        </div>
      </transition>

      <br />

      <!-- <b-button variant="danger" block size="lg" class="mt-4" @click="doDebug">
        Debug
      </b-button> -->

      <b-alert
        v-model="isBadEmail"
        variant="danger"
        dismissible
        @dismissed="doReset"
      >
        <!-- <h4 class="alert-heading">Invalid Email</h4> -->
        No account for {{ emailAddress }}, please try again.
      </b-alert>

      <b-alert v-model="isBadCode" variant="danger">
        <!-- <h4 class="alert-heading">Invalid Code</h4> -->
        That reset code is invalid, please try again.
      </b-alert>

      <b-alert
        v-model="isOkReset"
        variant="success"
        dismissible
        @dismissed="doLogin"
      >
        <!-- <h4 class="alert-heading">Password Successfully Updated</h4> -->
        Reset complete, please login with new password!
      </b-alert>
    </b-form>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { ResetInfo } from "@/types";
import ApiService from "@/services/api";

@Component
export default class Reset extends Vue {
  emailAddress = "";
  resetPassword = "";
  resetCode = "";
  isValidEmail = true;

  isSentResetCode = false;
  isSubmittedResetCode = false;
  isBadEmail = false;
  isBadCode = false;
  isOkReset = false;

  onSubmit() {
    const reset: ResetInfo = {
      emailAddress: this.emailAddress
    };
    ApiService.reset(reset)
      .then(() => {
        this.statusSentReset();
      })
      .catch(() => {
        this.statusBadEmail();
      });
  }

  statusSentReset() {
    this.isSentResetCode = true;
  }

  statusBadEmail() {
    this.isSentResetCode = false;
    this.isBadEmail = true;
  }

  statusBadCode() {
    this.isSubmittedResetCode = false;
    this.isBadCode = true;
  }

  statusOkReset() {
    this.isOkReset = true;
  }

  resetCancel() {
    this.isSentResetCode = false;
    this.isSubmittedResetCode = false;
    this.isBadEmail = false;
    this.isBadCode = false;
    this.isOkReset = false;
  }

  resetPass() {
    const resetInfo = {
      email: this.emailAddress,
      password: this.resetPassword,
      code: this.resetCode
    };
    this.isBadCode = false;
    this.isSubmittedResetCode = true;
    ApiService.resetPass(resetInfo)
      .then(() => {
        this.statusOkReset();
      })
      .catch(() => {
        this.statusBadCode();
      });
  }

  doReset() {
    this.isSentResetCode = false;
    this.isBadCode = false;
    this.isBadEmail = false;
    this.isOkReset = false;
  }

  doLogin() {
    return ApiService.unauthorized();
  }

  doDebug() {
    // this.$bvModal.show("reset-success-modal");
    // this.$bvModal.show("reset-failure-modal");
    // this.$bvModal.show("reset-unknown-modal");
    // this.$bvModal.show("reset-modal");
    this.isSentResetCode = !this.isSentResetCode;
    // this.statusBadCode();
    // this.statusBadEmail();
    // this.statusOkReset();
  }
}
</script>

<style lang="scss">
.reset {
  width: 100%;
  display: flex;
  align-items: center;
}

.form-reset {
  width: 100%;
  max-width: 500px;
  padding: 15px;
  margin: 0 auto;
}

.form-reset input[type="text"] {
  margin-bottom: -1px;
  border-bottom-right-radius: 0;
  border-bottom-left-radius: 0;
}

.form-reset input[type="password"] {
  margin-bottom: 10px;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}

.form-reset .form-control {
  height: auto;
  position: relative;
  box-sizing: border-box;
  padding: 10px;
  font-size: 16px;
}

.fade-enter-active,
.fade-leave-active {
  transition: opacity 0.5s;
}
.fade-enter, .fade-leave-to /* .fade-leave-active below version 2.1.8 */ {
  opacity: 0;
}
</style>
