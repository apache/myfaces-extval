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
package org.apache.myfaces.extensions.validator.baseval.message.resolver;

import org.apache.myfaces.extensions.validator.baseval.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.AbstractValidationErrorMessageResolver;

/**
 * @author Gerhard Petracek
 */
public class JpaValidationErrorMessageResolver extends AbstractValidationErrorMessageResolver
{
    private static final String CUSTOM_BASE_NAME = WebXmlParameter.VALIDATION_MESSAGES_JPA;
    private static final String BASE_NAME = JpaValidationErrorMessageResolver.class
            .getPackage().getName().replace(".message.resolver", ".message.bundle")+ ".jpa_messages";

    protected String getCustomBaseName()
    {
        return CUSTOM_BASE_NAME;
    }

    protected String getBaseName()
    {
        return BASE_NAME;
    }

}