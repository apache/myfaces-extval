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
package org.apache.myfaces.extensions.validator.core.validation.exception;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;

/**
 * JSF components allow different inline-messages for required violations and all other types of validation failures.
 * This exception is used to determine if the special required error message should be used. 
 *
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public class RequiredValidatorException extends ValidatorException
{
    private static final long serialVersionUID = -4646331736428495884L;

    public RequiredValidatorException(FacesMessage facesMessage)
    {
        super(facesMessage);
    }

    public RequiredValidatorException(FacesMessage facesMessage, Throwable throwable)
    {
        super(facesMessage, throwable);
    }
}
