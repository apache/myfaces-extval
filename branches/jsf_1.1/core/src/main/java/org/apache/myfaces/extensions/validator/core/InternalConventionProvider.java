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
package org.apache.myfaces.extensions.validator.core;

import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class InternalConventionProvider
{
    public static String getModuleMessageBundleName(String packageName)
    {
        String newPackageName;
        if (packageName.endsWith(".resolver"))
        {
            newPackageName = packageName.replace(".resolver", ".bundle");
        }
        else
        {
            newPackageName = packageName.replace(".resolver.", ".bundle.");
        }

        return newPackageName + ".validation_messages";
    }

    /**
     * use a custom name mapper to implement custom conventions
     */
    @ToDo(value = Priority.MEDIUM, description = "logging")
    public static String getMessageResolverClassName(
        Class<? extends ValidationStrategy> validationStrategyClass, String targetClassName)
    {
        return getValidationStrategyBasedName(validationStrategyClass, ".message.resolver.", targetClassName);
    }

    public static String getMetaDataTransformerClassName(
        Class<? extends ValidationStrategy> validationStrategyClass, String targetClassName)
    {
        return getValidationStrategyBasedName(validationStrategyClass, ".metadata.transformer.", targetClassName);
    }

    private static String getValidationStrategyBasedName(Class<? extends ValidationStrategy> validationStrategyClass,
                                                  String targetPackageName, String targetClassName)
    {
        String validationStrategyClassName = validationStrategyClass.getName();

        validationStrategyClassName = validationStrategyClassName.replace(".strategy.", targetPackageName);

        if (targetClassName == null)
        {
            return null;
        }
        return validationStrategyClassName
                .substring(0, validationStrategyClassName.lastIndexOf(".")) + "." + targetClassName;
    }

    /**
     * use a custom name mapper to implement custom conventions
     */
    public static String getMessageResolverClassName(String validationStrategyName)
    {
        return getValidationStrategyBasedName(validationStrategyName, "ValidationErrorMessageResolver");
    }

    /**
     * use a custom name mapper to implement custom conventions
     */
    public static String getValidationStrategyClassName(Annotation annotation)
    {
        return annotation.annotationType().getName().replace(".annotation.", ".strategy.") + "Strategy";
    }

    public static String getMetaDataTransformerClassName(String validationStrategyName)
    {
        return getValidationStrategyBasedName(validationStrategyName, "MetaDataTransformer");
    }

    private static String getValidationStrategyBasedName(String validationStrategyName, String targetPostfix)
    {
        if (validationStrategyName.endsWith("ValidationStrategy"))
        {
            return validationStrategyName.substring(0, validationStrategyName.length() - 18) + targetPostfix;
        }
        else if (validationStrategyName.endsWith("Strategy"))
        {
            return validationStrategyName.substring(0, validationStrategyName.length() - 8) + targetPostfix;
        }
        return null;
    }
}
