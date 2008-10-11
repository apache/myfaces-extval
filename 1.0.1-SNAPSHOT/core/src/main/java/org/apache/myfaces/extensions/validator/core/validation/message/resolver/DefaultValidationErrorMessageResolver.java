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
package org.apache.myfaces.extensions.validator.core.validation.message.resolver;

import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.InternalConventionProvider;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * Default MessageResolver which uses the default convention for the message bundle.
 * It's possible to provide a custom message bundle via web.xml
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.CUSTOMIZABLE})
public class DefaultValidationErrorMessageResolver extends
    AbstractValidationErrorMessageResolver
{
    private static final String CUSTOM_BUNDLE = WebXmlParameter.CUSTOM_MESSAGE_BUNDLE;

    //not used at the moment - just for a convention
    protected String getBaseName()
    {
        return InternalConventionProvider.getModuleMessageBundleName(getClass().getPackage().getName());
    }

    protected String getCustomBaseName()
    {
        return CUSTOM_BUNDLE;
    }
}
