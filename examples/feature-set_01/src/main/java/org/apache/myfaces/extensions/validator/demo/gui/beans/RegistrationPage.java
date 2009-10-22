/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.demo.gui.beans;

import org.apache.myfaces.extensions.validator.demo.domain.Person;

import org.apache.myfaces.extensions.validator.crossval.annotation.Equals;
import org.apache.myfaces.extensions.validator.crossval.annotation.NotEquals;
import org.apache.myfaces.extensions.validator.baseval.annotation.JoinValidation;
import org.apache.myfaces.extensions.validator.baseval.annotation.LongRange;
import org.apache.myfaces.extensions.validator.baseval.annotation.Required;
import org.apache.myfaces.extensions.validator.baseval.annotation.Validator;
import org.apache.myfaces.extensions.validator.baseval.annotation.SkipValidation;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.apache.myfaces.custom.emailvalidator.EmailValidator;

public class RegistrationPage
{

    //the old password of the person isn't used within the page
    //-> validate with value of the model
    @SkipValidation("#{person.role eq 'admin'}")
    @Required
    @Equals("person.password")
    @NotEquals("password")
    private String oldPassword;

    @Equals("passwordRepeated")
    //combine gui related annotations with the annoations of the domain model
    //@JoinValidation(value = "#{registrationPage.person.password}")
    private String password;

    @Required(validationErrorMsgKey = "repeated_password_required",
            parameters = {DisableClientSideValidation.class, ViolationSeverity.Warn.class})
    private String passwordRepeated;

    //use #{registrationPage.person.nickName}, #{person.nickName}
    //use registrationPage to display the second error message at old nickname
    //or local property chaining (to avoid the usage of the bean name) - like:
    @SkipValidation("#{person.role eq 'admin'}")
    @NotEquals("person.nickName")
    @JoinValidation("person.nickName")
    private String newNickName;

    private Person person;

    public String finish()
    {
        this.person.setPassword(this.password);
        return "home";
    }

    public String updateNickName()
    {
        this.person.setNickName(this.newNickName);
        return "home";
    }

    //just a quick illustration of skipValidation
    public String switchMode()
    {
        String currentRole = this.person.getRole();

        if(currentRole.equals("admin"))
        {
            this.person.setRole("user");
        }
        else
        {
            this.person.setRole("admin");
        }
        return "";
    }

    @JoinValidation("org.apache.myfaces.extensions.validator.demo.domain.Person:email")
    @Validator(EmailValidator.class)
    public String getEmail()
    {
        return this.person.getEmail();
    }

    public void setEmail(String email)
    {
        this.person.setEmail(email);
    }

    @JoinValidation("#{person.numberOfSiblings}")
    @LongRange(maximum = 20)
    public int getNumberOfSiblings()
    {
        return this.person.getNumberOfSiblings();
    }

    public void setNumberOfSiblings(int numberOfSiblings)
    {
        this.person.setNumberOfSiblings(numberOfSiblings);
    }

    /*
     * generated
     */
    public String getOldPassword()
    {
        return oldPassword;
    }

    public void setOldPassword(String oldPassword)
    {
        this.oldPassword = oldPassword;
    }

    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

    public String getPasswordRepeated()
    {
        return passwordRepeated;
    }

    public void setPasswordRepeated(String passwordRepeated)
    {
        this.passwordRepeated = passwordRepeated;
    }

    public Person getPerson()
    {
        return person;
    }

    public void setPerson(Person person)
    {
        this.person = person;
    }

    public String getNewNickName()
    {
        return newNickName;
    }

    public void setNewNickName(String newNickName)
    {
        this.newNickName = newNickName;
    }
}
