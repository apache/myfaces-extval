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

import org.apache.myfaces.extensions.validator.baseval.ExtValBaseValidationModuleConfiguration;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.AbstractValidationErrorMessageResolver;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
/*
 * the jpa support is an exception in view of some mechanisms - so there's no convention for the message bundle.
 * instead of the convention there is a global property to have an alternative to web.xml configuration
 */
@UsageInformation(UsageCategory.INTERNAL)
public class JpaValidationErrorMessageResolver extends AbstractValidationErrorMessageResolver
{
    public static final String JPA_VALIDATION_ERROR_MESSAGES = "JPA_VALIDATION_ERROR_MESSAGES";

    private static final String CUSTOM_BASE_NAME =
            ExtValBaseValidationModuleConfiguration.get().jpaValidationErrorMessages();
    
    private static final String BASE_NAME = JpaValidationErrorMessageResolver.class
            .getPackage().getName().replace(".message.resolver", ".message.bundle")+ ".jpa_messages";

    protected String getCustomBaseName()
    {
        if(CUSTOM_BASE_NAME != null)
        {
            return CUSTOM_BASE_NAME;
        }

        return (String)ExtValContext.getContext().getGlobalProperty(JPA_VALIDATION_ERROR_MESSAGES);
    }

    protected String getBaseName()
    {
        return BASE_NAME;
    }

}
