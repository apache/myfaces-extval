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
package org.apache.myfaces.extensions.validator.core.annotation.extractor;

import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInfo;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * This factory creates an annotation extractor which extracts the annotation
 * of the value binding of a component.
 * <p/>
 * order:<br/>
 * <ol>
 *   <li>configured annotation extractor (web.xml)</li>
 *   <li>configured annotation extractor (information provider bean)</li>
 *   <li>default implementation</li>
 * </ol>
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultComponentAnnotationExtractorFactory implements AnnotationExtractorFactory
{
    private final Log logger = LogFactory.getLog(getClass());

    private static AnnotationExtractor annotationExtractor = null;

    public DefaultComponentAnnotationExtractorFactory()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "logging")
    public AnnotationExtractor create()
    {
        if (annotationExtractor == null)
        {
            List<String> annotationExtractorClassNames = new ArrayList<String>();

            annotationExtractorClassNames.add(WebXmlParameter.CUSTOM_COMPONENT_ANNOTATION_EXTRACTOR);
            annotationExtractorClassNames
                .add(ExtValContext.getContext().getInformationProviderBean()
                    .get(CustomInfo.COMPONENT_ANNOTATION_EXTRACTOR));
            annotationExtractorClassNames.add(DefaultComponentAnnotationExtractor.class.getName());

            for (String className : annotationExtractorClassNames)
            {
                annotationExtractor = (AnnotationExtractor) ClassUtils.tryToInstantiateClassForName(className);

                if (annotationExtractor != null)
                {
                    break;
                }
            }
        }

        if(logger.isTraceEnabled())
        {
            logger.trace(annotationExtractor.getClass().getName() + " created");
        }

        return annotationExtractor;
    }
}
