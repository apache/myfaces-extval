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
package org.apache.myfaces.blank;

import org.apache.myfaces.blank.domain.Person;
import org.apache.myfaces.blank.validation.group.Admin;
import org.apache.myfaces.blank.validation.group.User;
import org.apache.myfaces.extensions.validator.beanval.annotation.BeanValidationController;
import org.apache.myfaces.extensions.validator.beanval.annotation.group.Group;
import org.apache.myfaces.extensions.validator.beanval.annotation.group.GroupValidation;

/**
 * A typical simple backing bean, that is backed to <code>helloworld.jsp</code>
 */

//@Group({User.class, Admin.class})
//in case of deactivated default validation
@BeanValidationController
public class HelloWorldController
{
    //@BeanValidationController(@GroupValidation(viewId = "/form1.jsp"))
    private Person person = new Person();

    /**
     * default empty constructor
     */
    public HelloWorldController()
    {
    }

    /**
     * Method that is backed to a submit button of a form.
     */
    public String send()
    {
        //do real logic, return a string which will be used for the navigation system of JSF
        return "success";
    }

    @BeanValidationController({
            @GroupValidation(viewId = "/helloWorld.jsp"),
            @GroupValidation(viewId = "/form1.jsp", use = @Group(User.class)),
            @GroupValidation(viewId = "/form2.jsp", use = {@Group(Admin.class)})
    })
    public Person getPerson()
    {
        return person;
    }

    public void setPerson(Person person)
    {
        this.person = person;
    }
}