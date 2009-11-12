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
package org.apache.myfaces.extensions.validator.beanval.util;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.JsfUtils;

import java.util.MissingResourceException;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
class LabeledMessageInternals
{
    //there is no concurrency issue here - it always leads to the same result
    private final String defaultLabelMessageTemplate = "{1}: {0}";
    private String labelMessageTemplate = defaultLabelMessageTemplate;
    private static final String JAVAX_FACES_VALIDATOR_BEANVALIDATOR_MESSAGE =
            "javax.faces.validator.BeanValidator.MESSAGE";

    String createLabeledMessage(String violationMessage)
    {
        if(labelMessageTemplate == null)
        {
            return this.defaultLabelMessageTemplate.replace("{0}", violationMessage);
        }

        this.labelMessageTemplate = loadStandardMessageTemplate();

        if(labelMessageTemplate == null)
        {
            return createLabeledMessage(violationMessage);
        }
        return labelMessageTemplate.replace("{0}", violationMessage);
    }

    private String loadStandardMessageTemplate()
    {
        try
        {
            return JsfUtils.getDefaultFacesMessageBundle().getString(JAVAX_FACES_VALIDATOR_BEANVALIDATOR_MESSAGE);
        }
        catch (MissingResourceException e)
        {
            return null;
        }
    }
}
