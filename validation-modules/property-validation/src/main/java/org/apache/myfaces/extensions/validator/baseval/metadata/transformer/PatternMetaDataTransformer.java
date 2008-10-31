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
package org.apache.myfaces.extensions.validator.baseval.metadata.transformer;

import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.baseval.annotation.Pattern;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.context.FacesContext;
import java.util.HashMap;
import java.util.Map;
import java.util.Locale;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class PatternMetaDataTransformer implements MetaDataTransformer
{
    public Map<String, Object> convertMetaData(MetaDataEntry metaDataEntry)
    {
        Map<String, Object> results = new HashMap<String, Object>();
        Pattern annotation = metaDataEntry.getValue(Pattern.class);
        
        results.put(CommonMetaDataKeys.PATTERN, (annotation).value());

        String validationErrorMsgKey = (annotation).validationErrorMsgKey();
        Locale currentLocale = FacesContext.getCurrentInstance().getViewRoot().getLocale();

        ValidationStrategy validationStrategy =
            ExtValUtils.getValidationStrategyForMetaData(annotation.annotationType().getName());

        String validationErrorMsg = ExtValUtils.getMessageResolverForValidationStrategy(validationStrategy)
            .getMessage(validationErrorMsgKey, currentLocale);

        results.put(CommonMetaDataKeys.PATTERN_VALIDATION_ERROR_MESSAGE, validationErrorMsg);
        return results;
    }
}
